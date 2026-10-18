package com.bencodez.votingplugin.tests.discord;

import com.bencodez.votingplugin.discord.*;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;
import java.util.function.Consumer;
import java.util.logging.Logger;
import org.junit.jupiter.api.Test;
import com.bencodez.votingplugin.VotingPluginMain;
import com.bencodez.votingplugin.config.Config;
import com.bencodez.votingplugin.data.ServerData;
import com.bencodez.votingplugin.topvoter.*;
import github.scarsz.discordsrv.dependencies.jda.api.entities.*;
import github.scarsz.discordsrv.dependencies.jda.api.requests.restaction.MessageAction;
import github.scarsz.discordsrv.dependencies.jda.api.requests.restaction.AuditableRestAction;
import github.scarsz.discordsrv.dependencies.jda.api.requests.ErrorResponse;
import github.scarsz.discordsrv.dependencies.jda.api.exceptions.ErrorResponseException;

class DiscordLeaderboardRecoveryTest {
    static final class Fixture implements AutoCloseable {
        final VotingPluginMain plugin=mock(VotingPluginMain.class);
        final Logger logger=mock(Logger.class);
        final Config config=mock(Config.class);final ServerData data=mock(ServerData.class);
        final TextChannel channel=mock(TextChannel.class);
        final MessageAction edit=mock(MessageAction.class),send=mock(MessageAction.class);
        final AtomicLong generation=new AtomicLong(), storedId=new AtomicLong(9L);
        final ArrayDeque<Runnable> worker=new ArrayDeque<>();
        final LinkedHashMap<TopVoterPlayer,Integer> board=new LinkedHashMap<>();
        final TopVoterPlayer first=new TopVoterPlayer(new UUID(0,1),"SyntheticFirst",1L);
        final TopVoterPlayer second=new TopVoterPlayer(new UUID(0,2),"SyntheticSecond",1L);
        Consumer<Throwable> editFailure,sendFailure;Consumer<Message> sendSuccess,editSuccess;MessageEmbed sent;
        Runnable beforeChannelLookup = () -> { };
        final DiscordHandler handler;
        Fixture() {
            when(plugin.getConfigFile()).thenReturn(config);when(plugin.getServerData()).thenReturn(data);
            when(plugin.getLogger()).thenReturn(logger);when(plugin.isEnabled()).thenReturn(true);
            when(plugin.getTopVoterDisplayGeneration()).thenAnswer(call->generation.get());
            when(plugin.getTopVoterDisplay(any())).thenAnswer(call->new LinkedHashMap<>(board));
            when(config.getDiscordSRVTopVoterChannel(any())).thenReturn(1L);
            when(config.getDiscordSRVTopVoterTitle(any())).thenReturn("Synthetic board");
            when(config.isDiscordSRVTopVoterAutoRecoverMessageOnFailure()).thenReturn(true);
            when(data.getTopVoterMessageId(any())).thenAnswer(call->storedId.get());
            doAnswer(call->{storedId.set(call.getArgument(1));return null;})
                    .when(data).setTopVoterMessageId(any(),anyLong());
            doAnswer(call->{data.setTopVoterMessageId(call.getArgument(0),call.getArgument(1));return true;})
                    .when(data).setTopVoterMessageIdConfirmed(any(),anyLong());
            var timer=mock(ScheduledExecutorService.class);when(plugin.getTimer()).thenReturn(timer);
            doAnswer(call->{worker.add(call.getArgument(0));return null;}).when(timer).execute(any(Runnable.class));
            when(channel.editMessageEmbedsById(anyLong(),any(MessageEmbed.class))).thenReturn(edit);
            when(channel.sendMessageEmbeds(any(MessageEmbed.class))).thenAnswer(call->{sent=call.getArgument(0);return send;});
            doAnswer(call->{editSuccess=call.getArgument(0);editFailure=call.getArgument(1);return null;}).when(edit).queue(any(),any());
            doAnswer(call->{sendSuccess=call.getArgument(0);sendFailure=call.getArgument(1);return null;}).when(send).queue(any(),any());
            board.put(second,90);handler=spy(new DiscordHandler(plugin));
            DiscordHandlerAccess.channel(handler, channelId -> {
                assertEquals(1L,channelId);
                var hook = beforeChannelLookup; beforeChannelLookup = () -> { }; hook.run();
                return channel;
            });
        }
        void publish() {DiscordHandlerAccess.publish(handler,TopVoter.Monthly,new LinkedHashMap<>(board));assertNotNull(editFailure);}
        void unknown() {var error=mock(ErrorResponseException.class);when(error.getErrorResponse()).thenReturn(ErrorResponse.UNKNOWN_MESSAGE);editFailure.accept(error);}
        void drain() {int bound=20;while(!worker.isEmpty()){assertTrue(bound-->0);worker.remove().run();}}
        public void close(){}
    }

    private static final class CreatedMessage {
        final Message message=mock(Message.class);
        @SuppressWarnings("unchecked")
        final AuditableRestAction<Void> deletion=mock(AuditableRestAction.class);
        CreatedMessage(long id) {
            when(message.getIdLong()).thenReturn(id);
            when(message.delete()).thenReturn(deletion);
        }
        void assertDeletionQueued() {
            verify(message,times(1)).delete();
            verify(deletion,times(1)).queue(any(),any());
        }
    }

    @Test void clearedIdDoesNotDeleteIntentionallyDetachedPost() {
        try(var f=new Fixture()) {
            startPersistentSend(f,false);var first=new CreatedMessage(12L);
            f.sendSuccess.accept(first.message);f.drain();
            // Existing ClearDiscordMessageID command semantics: detach, leave the post intact.
            assertTrue(f.handler.clearTopVoterMessageId(TopVoter.Monthly));
            DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);
            var next=new CreatedMessage(13L);f.sendSuccess.accept(next.message);f.drain();
            assertEquals(13L,f.storedId.get());verify(first.message,never()).delete();
        }
    }

    @Test void clearDetachesSuccessfulPendingSendsBeforeCompletionOrCancellation() {
        for(boolean recovery:List.of(false,true))for(int retirement=0;retirement<3;retirement++)try(var f=new Fixture()) {
            startPersistentSend(f,recovery);var created=new CreatedMessage(12L);var success=f.sendSuccess;
            // The clear command was queued first; the send succeeds before either worker action executes.
            f.worker.add(()->assertTrue(f.handler.clearTopVoterMessageId(TopVoter.Monthly)));
            success.accept(created.message);assertEquals(2,f.worker.size());f.worker.remove().run();
            assertEquals(0L,f.storedId.get());
            if(retirement==0)f.drain();
            else {
                if(retirement==1){f.generation.incrementAndGet();f.handler.retireLeaderboardPublications();}
                else f.handler.close();
                f.worker.clear();
            }
            success.accept(created.message);verify(created.message,never()).delete();
            verify(f.data,never()).setTopVoterMessageId(TopVoter.Monthly,12L);
            if(retirement!=2) {
                DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);
                var next=new CreatedMessage(13L);f.sendSuccess.accept(next.message);f.drain();
                assertEquals(13L,f.storedId.get());verify(created.message,never()).delete();
                verify(next.message,never()).delete();
            }
        }
    }

    @Test void failedClearDoesNotDeleteTheAlreadySuccessfulPendingPost() {
        for(boolean recovery:List.of(false,true))try(var f=new Fixture()) {
            startPersistentSend(f,recovery);var created=new CreatedMessage(12L);f.sendSuccess.accept(created.message);
            doReturn(false).when(f.data).setTopVoterMessageIdConfirmed(TopVoter.Monthly,0L);
            assertFalse(f.handler.clearTopVoterMessageId(TopVoter.Monthly));f.drain();
            verify(created.message,never()).delete();
            verify(f.data,never()).setTopVoterMessageId(TopVoter.Monthly,12L);
            verify(f.logger).warning(contains("storage"));
        }
    }

    @Test void clearingOneRankingDetachesOnlyItsPendingSuccessfulPosts() {
        try(var f=new Fixture()) {
            startPersistentSend(f,false);var monthly=new CreatedMessage(12L);f.sendSuccess.accept(monthly.message);
            f.handler.getTopVoterMessageIds().put(TopVoter.Weekly,0L);
            DiscordHandlerAccess.publish(f.handler,TopVoter.Weekly,f.board);
            var weekly=new CreatedMessage(13L);f.sendSuccess.accept(weekly.message);
            assertTrue(f.handler.clearTopVoterMessageId(TopVoter.Monthly));
            f.handler.close();f.worker.clear();
            verify(monthly.message,never()).delete();weekly.assertDeletionQueued();
            verify(f.data,never()).setTopVoterMessageId(TopVoter.Monthly,12L);
            verify(f.data,never()).setTopVoterMessageId(TopVoter.Weekly,13L);
        }
    }

    @Test void successReportedOnlyAfterClearIsStillReclaimedAsAnObsoletePublication() {
        for(boolean recovery:List.of(false,true))try(var f=new Fixture()) {
            startPersistentSend(f,recovery);var success=f.sendSuccess;
            assertTrue(f.handler.clearTopVoterMessageId(TopVoter.Monthly));
            var created=new CreatedMessage(12L);success.accept(created.message);f.drain();
            created.assertDeletionQueued();
            verify(f.data,never()).setTopVoterMessageId(TopVoter.Monthly,12L);
            assertEquals(0L,f.storedId.get());
        }
    }

    @Test void swallowedRealFileSaveFailureRequiresStorageReconciliation(
            @org.junit.jupiter.api.io.TempDir java.nio.file.Path directory) throws Exception {
        try(var f=new Fixture()) {
            var file=mock(com.bencodez.advancedcore.data.ServerData.class,CALLS_REAL_METHODS);
            var yaml=new org.bukkit.configuration.file.YamlConfiguration();
            var dataField=com.bencodez.simpleapi.file.YMLFile.class.getDeclaredField("data");
            dataField.setAccessible(true);dataField.set(file,yaml);
            var pathField=com.bencodez.simpleapi.file.YMLFile.class.getDeclaredField("dFile");
            pathField.setAccessible(true);pathField.set(file,directory.toFile());
            when(f.plugin.getServerDataFile()).thenReturn(file);
            when(f.plugin.getServerData()).thenReturn(new ServerData(f.plugin));
            startPersistentSend(f,false);var created=new CreatedMessage(12L);
            f.sendSuccess.accept(created.message);f.drain();
            assertEquals(12L,yaml.getLong("VotingPlugin.DiscordSRV.TopVoterMessageId.Monthly"));
            assertTrue(java.nio.file.Files.isDirectory(directory));
            verify(f.logger).warning(argThat((String text)->text.contains("storage")
                    &&!text.contains(directory.toString())));
            verify(created.message,never()).delete();
        }
    }

    @Test void cancelledAcceptedCompletionsAreReconciledBeforeWorkerTeardown() {
        for(boolean recovery:List.of(false,true))for(boolean shutdown:List.of(false,true))try(var f=new Fixture()) {
            startPersistentSend(f,recovery);var created=new CreatedMessage(12L);
            f.sendSuccess.accept(created.message);assertFalse(f.worker.isEmpty());
            if(shutdown)f.handler.close();else {f.generation.incrementAndGet();f.handler.retireLeaderboardPublications();}
            f.worker.clear(); // The executor accepted the task, then discarded it without invoking the closure.
            created.assertDeletionQueued();verify(f.data,never()).setTopVoterMessageId(TopVoter.Monthly,12L);
            if(shutdown){f.handler.close();f.sendSuccess.accept(created.message);created.assertDeletionQueued();}
            else {
                DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);
                var next=new CreatedMessage(13L);f.sendSuccess.accept(next.message);f.drain();
                assertEquals(13L,f.storedId.get());verify(next.message,never()).delete();
            }
        }
    }

    @Test void shutdownDoesNotDeleteRecordedOrIntentionallyAppendedPosts() {
        for(boolean append:List.of(false,true))try(var f=new Fixture()) {
            if(append)when(f.config.isDiscordSRVTopVoterNewMessageOnUpdate(any())).thenReturn(true);
            startPersistentSend(f,false);var created=new CreatedMessage(12L);
            f.sendSuccess.accept(created.message);if(!append)f.drain();f.handler.close();f.worker.clear();
            verify(created.message,never()).delete();
        }
    }

    @Test void shutdownDuringAStartedWriteKeepsItsConfirmedReceipt() {
        try(var f=new Fixture()) {
            startPersistentSend(f,false);var created=new CreatedMessage(12L);
            doAnswer(call->{f.handler.close();f.storedId.set(12L);return null;})
                    .when(f.data).setTopVoterMessageId(TopVoter.Monthly,12L);
            f.sendSuccess.accept(created.message);f.drain();
            assertEquals(12L,f.storedId.get());verify(created.message,never()).delete();
            verify(f.logger).warning(contains("storage"));
        }
    }

    @Test void lateSuccessAfterShutdownAndRejectedCleanupRemainObservable() {
        try(var f=new Fixture()) {
            startPersistentSend(f,false);var created=new CreatedMessage(12L);f.handler.close();
            when(created.message.delete()).thenThrow(new IllegalStateException("synthetic-provider-content"));
            f.sendSuccess.accept(created.message);f.drain();
            verify(f.logger).warning(argThat((String text)->text.contains("untracked")&&!text.contains("synthetic-provider-content")));
            verify(f.data,never()).setTopVoterMessageId(TopVoter.Monthly,12L);
        }
    }

    @Test void uncertainSavesKeepAllReceiptsUntilAConfirmedReplacement() {
        try(var f=new Fixture()) {
            doAnswer(call->{f.data.setTopVoterMessageId(call.getArgument(0),call.getArgument(1));return false;})
                    .when(f.data).setTopVoterMessageIdConfirmed(any(),anyLong());
            startPersistentSend(f,false);var first=new CreatedMessage(12L);f.sendSuccess.accept(first.message);f.drain();
            f.generation.incrementAndGet();f.handler.getTopVoterMessageIds().put(TopVoter.Monthly,0L);
            DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);
            var second=new CreatedMessage(13L);f.sendSuccess.accept(second.message);f.drain();
            verify(first.message,never()).delete();verify(second.message,never()).delete();
            doAnswer(call->{f.data.setTopVoterMessageId(call.getArgument(0),call.getArgument(1));return true;})
                    .when(f.data).setTopVoterMessageIdConfirmed(any(),anyLong());
            DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);f.editSuccess.accept(second.message);f.drain();
            first.assertDeletionQueued();verify(second.message,never()).delete();assertEquals(13L,f.storedId.get());
        }
    }

    @Test void uncertainReceiptCapacityRefusesAnotherSaveAndReclaimsOnlyTheNewPost() {
        try(var f=new Fixture()) {
            doAnswer(call->{f.data.setTopVoterMessageId(call.getArgument(0),call.getArgument(1));return false;})
                    .when(f.data).setTopVoterMessageIdConfirmed(any(),anyLong());
            var posts=new ArrayList<CreatedMessage>();
            for(int index=0;index<129;index++) {
                f.generation.incrementAndGet();f.handler.getTopVoterMessageIds().put(TopVoter.Monthly,0L);
                DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);
                var created=new CreatedMessage(1000L+index);posts.add(created);f.sendSuccess.accept(created.message);f.drain();
            }
            for(int index=0;index<128;index++)verify(posts.get(index).message,never()).delete();
            posts.get(128).assertDeletionQueued();assertEquals(1127L,f.storedId.get());
            verify(f.data,never()).setTopVoterMessageId(TopVoter.Monthly,1128L);
            assertFalse(f.handler.clearTopVoterMessageId(TopVoter.Monthly));
            for(int index=0;index<128;index++)verify(posts.get(index).message,never()).delete();
        }
    }

    @Test void clearDuringAReentrantStartedWriteKeepsItsPostDetached() {
        try(var f=new Fixture()) {
            startPersistentSend(f,false);var created=new CreatedMessage(12L);
            doAnswer(call->{f.storedId.set(12L);assertTrue(f.handler.clearTopVoterMessageId(TopVoter.Monthly));return null;})
                    .when(f.data).setTopVoterMessageId(TopVoter.Monthly,12L);
            f.sendSuccess.accept(created.message);f.drain();
            assertEquals(0L,f.storedId.get());assertEquals(0L,f.handler.getTopVoterMessageIds().get(TopVoter.Monthly));
            DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);
            var next=new CreatedMessage(13L);f.sendSuccess.accept(next.message);f.drain();
            verify(created.message,never()).delete();assertEquals(13L,f.storedId.get());
        }
    }

    @Test void anUnconfirmedNewerWriteKeepsBothReceiptsUntilConfirmation() {
        try(var f=new Fixture()) {
            startPersistentSend(f,false);var first=new CreatedMessage(12L);var next=new CreatedMessage(13L);
            doAnswer(call->{f.data.setTopVoterMessageId(call.getArgument(0),call.getArgument(1));return false;})
                    .when(f.data).setTopVoterMessageIdConfirmed(TopVoter.Monthly,13L);
            doAnswer(call->{
                f.storedId.set(12L);f.generation.incrementAndGet();
                DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);
                f.sendSuccess.accept(next.message);f.drain();return null;
            }).when(f.data).setTopVoterMessageId(TopVoter.Monthly,12L);
            f.sendSuccess.accept(first.message);f.drain();
            verify(first.message,never()).delete();verify(next.message,never()).delete();
            doAnswer(call->{f.data.setTopVoterMessageId(call.getArgument(0),call.getArgument(1));return true;})
                    .when(f.data).setTopVoterMessageIdConfirmed(TopVoter.Monthly,13L);
            DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);f.editSuccess.accept(next.message);f.drain();
            first.assertDeletionQueued();verify(next.message,never()).delete();assertEquals(13L,f.storedId.get());
        }
    }

    @Test void shutdownDoesNotWaitForAStartedWriteHoldingTheSaveMonitor() throws Exception {
        try(var f=new Fixture()) {
            startPersistentSend(f,false);var created=new CreatedMessage(12L);
            var entered=new CountDownLatch(1);var release=new CountDownLatch(1);var error=new AtomicReference<Throwable>();
            doAnswer(call->{entered.countDown();assertTrue(release.await(5,TimeUnit.SECONDS));f.storedId.set(12L);return null;})
                    .when(f.data).setTopVoterMessageId(TopVoter.Monthly,12L);
            f.sendSuccess.accept(created.message);var action=f.worker.remove();
            Thread worker=new Thread(()->{try{action.run();}catch(Throwable failure){error.set(failure);}});
            worker.start();
            try {
                assertTrue(entered.await(5,TimeUnit.SECONDS));
                assertTimeoutPreemptively(java.time.Duration.ofSeconds(1),f.handler::close);
                verify(created.message,never()).delete();
            } finally {release.countDown();worker.join(5000);}
            assertFalse(worker.isAlive());assertNull(error.get());assertEquals(12L,f.storedId.get());
            verify(created.message,never()).delete();verify(f.logger).warning(contains("storage"));
        }
    }

    private static void startPersistentSend(Fixture f,boolean recovery) {
        if(recovery){f.publish();f.unknown();f.drain();}
        else {
            f.handler.getTopVoterMessageIds().put(TopVoter.Monthly,0L);
            DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);
        }
        assertNotNull(f.sendSuccess);
    }

    @Test void discardedPersistentSuccessReclaimsItsCreatedMessageAcrossAdmissionAndRetirement() {
        for(boolean recovery:List.of(false,true))for(int timing=0;timing<4;timing++)try(var f=new Fixture()) {
            startPersistentSend(f,recovery);
            var created=new CreatedMessage(12L);
            var success=f.sendSuccess;
            if(timing==0)f.generation.incrementAndGet();
            var timer=f.plugin.getTimer();
            if(timing==2)doThrow(new RejectedExecutionException("synthetic rejected completion"))
                    .when(timer).execute(any(Runnable.class));
            if(timing==3)when(f.plugin.isEnabled()).thenReturn(false);
            success.accept(created.message);
            if(timing==1)f.generation.incrementAndGet();
            f.drain();success.accept(created.message);f.drain();
            created.assertDeletionQueued();
            verify(f.data,never()).setTopVoterMessageId(TopVoter.Monthly,12L);
        }
    }

    @Test void retiredSuccessCleansOnlyItsOwnMessageAndNeverTheSuccessors() {
        for(boolean recovery:List.of(false,true))try(var f=new Fixture()) {
            startPersistentSend(f,recovery);var old=f.sendSuccess;
            f.generation.incrementAndGet();
            DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);
            var successor=new CreatedMessage(13L);f.sendSuccess.accept(successor.message);f.drain();
            var retired=new CreatedMessage(12L);old.accept(retired.message);f.drain();
            assertEquals(13L,f.handler.getTopVoterMessageIds().get(TopVoter.Monthly));
            assertEquals(13L,f.storedId.get());retired.assertDeletionQueued();
            verify(successor.message,never()).delete();
        }
    }

    @Test void successfulPersistenceDuringRetirementRetainsTheCreatedIdForLaterUpdates() {
        try(var f=new Fixture()) {
            startPersistentSend(f,false);
            doAnswer(call->{f.storedId.set(12L);f.generation.incrementAndGet();return null;})
                    .when(f.data).setTopVoterMessageId(TopVoter.Monthly,12L);
            var created=new CreatedMessage(12L);f.sendSuccess.accept(created.message);f.drain();
            assertEquals(12L,f.handler.getTopVoterMessageIds().get(TopVoter.Monthly));
            assertEquals(12L,f.storedId.get());verify(created.message,never()).delete();
            DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);
            verify(f.channel).editMessageEmbedsById(eq(12L),any(MessageEmbed.class));
            verify(f.channel,times(1)).sendMessageEmbeds(any(MessageEmbed.class));
        }
    }

    @Test void overlappingSuccessorSendRetainsTheFirstReceiptUntilItCanBeReplaced() {
        for(boolean successorSucceeds:List.of(false,true))try(var f=new Fixture()) {
            startPersistentSend(f,false);
            doAnswer(call->{
                f.storedId.set(12L);f.generation.incrementAndGet();
                DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);
                return null;
            }).when(f.data).setTopVoterMessageId(TopVoter.Monthly,12L);
            var first=new CreatedMessage(12L);f.sendSuccess.accept(first.message);f.drain();
            assertEquals(12L,f.handler.getTopVoterMessageIds().get(TopVoter.Monthly));
            if(successorSucceeds) {
                var next=new CreatedMessage(13L);f.sendSuccess.accept(next.message);f.drain();
                assertEquals(13L,f.storedId.get());first.assertDeletionQueued();
                verify(next.message,never()).delete();
            } else {
                f.sendFailure.accept(new IllegalStateException("synthetic successor failure"));f.drain();
                assertEquals(12L,f.storedId.get());verify(first.message,never()).delete();
                DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);
                verify(f.channel).editMessageEmbedsById(eq(12L),any(MessageEmbed.class));
            }
        }
    }

    @Test void idPersistenceFailureRetainsObservableOwnershipAndAcceptedDuplicatesDoNotDelete() {
        for(boolean failed:List.of(false,true))try(var f=new Fixture()) {
            startPersistentSend(f,false);
            if(failed)doThrow(new IllegalStateException("synthetic failed ID write"))
                    .when(f.data).setTopVoterMessageId(TopVoter.Monthly,12L);
            var created=new CreatedMessage(12L);var success=f.sendSuccess;
            success.accept(created.message);f.drain();success.accept(created.message);f.drain();
            verify(created.message,never()).delete();
            if(failed)verify(f.logger).warning(contains("storage"));
        }
    }

    @Test void cleanupSubmissionAndNetworkFailuresAreObservableWithoutPrivateProviderDetails() {
        for(boolean synchronous:List.of(false,true))try(var f=new Fixture()) {
            startPersistentSend(f,false);f.generation.incrementAndGet();var created=new CreatedMessage(12L);
            if(synchronous)when(created.message.delete()).thenThrow(new IllegalStateException("synthetic-private-provider-detail"));
            else doAnswer(call->{
                Consumer<Throwable> failed=call.getArgument(1);
                failed.accept(new IllegalStateException("synthetic-private-provider-detail"));return null;
            }).when(created.deletion).queue(any(),any());
            assertDoesNotThrow(()->f.sendSuccess.accept(created.message));f.drain();
            verify(f.logger).warning(argThat((String text)->text.contains("untracked")
                    &&!text.contains("synthetic-private-provider-detail")));
        }
    }

    @Test void intentionalAppendModeDoesNotDeleteRetiredPosts() {
        try(var f=new Fixture()) {
            when(f.config.isDiscordSRVTopVoterNewMessageOnUpdate(any())).thenReturn(true);
            DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);
            f.generation.incrementAndGet();var created=new CreatedMessage(12L);
            f.sendSuccess.accept(created.message);f.drain();verify(created.message,never()).delete();
            verify(f.data,never()).setTopVoterMessageId(any(),anyLong());
        }
    }

    @Test void aNewerIdRecordedReentrantlyCannotBeOverwrittenByTheOlderCompletion() {
        try(var f=new Fixture()) {
            startPersistentSend(f,false);var first=new CreatedMessage(12L);var next=new CreatedMessage(13L);
            doAnswer(call->{
                f.storedId.set(12L);f.generation.incrementAndGet();
                DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);
                f.sendSuccess.accept(next.message);f.drain();return null;
            }).when(f.data).setTopVoterMessageId(TopVoter.Monthly,12L);
            f.sendSuccess.accept(first.message);f.drain();
            assertEquals(13L,f.storedId.get());
            assertEquals(13L,f.handler.getTopVoterMessageIds().get(TopVoter.Monthly));
            first.assertDeletionQueued();verify(next.message,never()).delete();
        }
    }

    @Test void cleanupOfAnAlreadyDeletedMessageIsAnIdempotentSuccess() {
        try(var f=new Fixture()) {
            startPersistentSend(f,false);f.generation.incrementAndGet();var created=new CreatedMessage(12L);
            var missing=mock(ErrorResponseException.class);when(missing.getErrorResponse()).thenReturn(ErrorResponse.UNKNOWN_MESSAGE);
            doAnswer(call->{Consumer<Throwable> failed=call.getArgument(1);failed.accept(missing);return null;})
                    .when(created.deletion).queue(any(),any());
            f.sendSuccess.accept(created.message);f.drain();created.assertDeletionQueued();
            verifyNoInteractions(f.logger);
        }
    }
    @Test void reloadDuringFinalProjectionCannotPromoteAnOlderPreparedPublication() {
        try (var f = new Fixture()) {
            when(f.plugin.getTopVoterDisplayAsync(any())).thenReturn(CompletableFuture.completedFuture(f.board));
            var reload = new AtomicBoolean(true);
            when(f.plugin.getTopVoterDisplay(any())).thenAnswer(call -> {
                if (reload.getAndSet(false)) f.generation.incrementAndGet();
                return new LinkedHashMap<>(f.board);
            });
            f.handler.updateTopVoterMessageId(TopVoter.Monthly); f.drain();
            verifyNoInteractions(f.channel);
            verify(f.data, never()).setTopVoterMessageId(any(), anyLong());
        }
    }
    @Test void reloadDuringCoalescedProjectionCannotPromoteAnOlderNetworkRequest() {
        try (var f = new Fixture()) {
            when(f.config.isDiscordSRVTopVoterNewMessageOnUpdate(any())).thenReturn(true);
            DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly, f.board);
            DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly, f.board);
            var reload = new AtomicBoolean(true);
            when(f.plugin.getTopVoterDisplay(any())).thenAnswer(call -> {
                if (reload.getAndSet(false)) f.generation.incrementAndGet();
                return new LinkedHashMap<>(f.board);
            });
            f.sendFailure.accept(new IllegalStateException("synthetic transient failure")); f.drain();
            verify(f.channel, times(1)).sendMessageEmbeds(any(MessageEmbed.class));
            verify(f.data, never()).setTopVoterMessageId(any(), anyLong());
        }
    }
    @Test void olderChannelLookupCannotReplaceANewerPublicationOwner() {
        try (var f = new Fixture()) {
            f.handler.getTopVoterMessageIds().put(TopVoter.Monthly, 0L);
            f.beforeChannelLookup = () -> {
                f.generation.incrementAndGet();
                DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly, f.board);
            };
            DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly, f.board);
            assertNotNull(f.sendSuccess);
            var message = mock(Message.class); when(message.getIdLong()).thenReturn(44L);
            f.sendSuccess.accept(message); f.drain();
            assertEquals(44L, f.handler.getTopVoterMessageIds().get(TopVoter.Monthly));
            verify(f.data).setTopVoterMessageId(TopVoter.Monthly, 44L);
            verify(f.channel, times(1)).sendMessageEmbeds(any(MessageEmbed.class));
        }
    }
    @Test void failedSendStillPublishesOneExplicitCoalescedRefreshInEverySendMode() {
        for(int mode=0;mode<3;mode++)try(var f=new Fixture()) {
            if(mode==0)when(f.config.isDiscordSRVTopVoterNewMessageOnUpdate(any())).thenReturn(true);
            if(mode==1)f.handler.getTopVoterMessageIds().put(TopVoter.Monthly,0L);
            if(mode==2){f.publish();f.unknown();f.drain();}
            else DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);
            f.board.clear();f.board.put(f.first,100);
            DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);
            var failed=f.sendFailure;failed.accept(new IllegalStateException("synthetic transient failure"));f.drain();
            verify(f.channel,times(2)).sendMessageEmbeds(any(MessageEmbed.class));
            assertTrue(f.sent.getDescription().contains("SyntheticFirst"));
            f.sendFailure.accept(new IllegalStateException("synthetic repeated failure"));f.drain();
            verify(f.channel,times(2)).sendMessageEmbeds(any(MessageEmbed.class));
        }
    }
    @Test void failedPublicationWarnsWithoutEchoingExceptionContent() {
        for(boolean send:List.of(false,true))try(var f=new Fixture()) {
            if(send){when(f.config.isDiscordSRVTopVoterNewMessageOnUpdate(any())).thenReturn(true);
                DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);
                f.sendFailure.accept(new IllegalStateException("synthetic-private-detail"));}
            else {f.publish();f.editFailure.accept(new IllegalStateException("synthetic-private-detail"));}
            verify(f.logger).warning(argThat((String text)->text.contains("Discord")&&!text.contains("synthetic-private-detail")));
        }
    }
    @Test void staleFailedSendCannotFlushAnOlderCoalescedRequest() {
        try(var f=new Fixture()) {
            when(f.config.isDiscordSRVTopVoterNewMessageOnUpdate(any())).thenReturn(true);
            DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);
            DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);
            f.generation.incrementAndGet();f.sendFailure.accept(new IllegalStateException("synthetic stale failure"));f.drain();
            verify(f.channel,times(1)).sendMessageEmbeds(any(MessageEmbed.class));verifyNoInteractions(f.logger);
        }
    }
    @Test void missingChannelBuildAndRecoveryFailuresHaveRedactedDiagnostics() {
        for(int mode=0;mode<3;mode++)try(var f=new Fixture()) {
            if(mode==0) {
                var absent=spy(new DiscordHandler(f.plugin)); DiscordHandlerAccess.channel(absent, id -> null);
                DiscordHandlerAccess.publish(absent,TopVoter.Monthly,f.board);
            } else if(mode==1) {
                when(f.config.getDiscordSRVTopVoterTitle(any())).thenThrow(new IllegalStateException("synthetic-private-detail"));
                DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);
            } else {
                doThrow(new IllegalStateException("synthetic-private-detail")).when(f.data).setTopVoterMessageId(TopVoter.Monthly,0L);
                f.publish();f.unknown();f.drain();
            }
            verify(f.logger).warning(argThat((String text)->text.contains("Discord")&&!text.contains("synthetic-private-detail")));
        }
    }
    @Test void recoveryReprojectsCurrentEvidenceAfterJoinAndWorkerQueueDelay() {
        try(var f=new Fixture()) {
            f.publish();f.unknown();assertNull(f.sent,"Recovery belongs on the existing worker");
            var current=new LinkedHashMap<TopVoterPlayer,Integer>();current.put(f.first,100);current.putAll(f.board);f.board.clear();f.board.putAll(current);
            f.drain();assertNotNull(f.sent);assertTrue(f.sent.getDescription().contains("SyntheticFirst"));
            verify(f.plugin,times(2)).getTopVoterDisplay(TopVoter.Monthly);
        }
    }
    @Test void reloadOrDisableBeforeErrorCannotSendOrClearCurrentMessageId() {
        for(boolean reload:List.of(true,false))try(var f=new Fixture()) {
            f.publish();if(reload)f.generation.incrementAndGet();else when(f.plugin.isEnabled()).thenReturn(false);
            f.unknown();f.drain();assertNull(f.sent);verify(f.data,never()).setTopVoterMessageId(any(),anyLong());
        }
    }
    @Test void reloadBeforeQueuedRecoveryDropsOldWork() {
        try(var f=new Fixture()) {f.publish();f.unknown();f.generation.incrementAndGet();f.drain();assertNull(f.sent);}
    }
    @Test void lateRecoverySuccessCannotOverwriteNewGenerationMessageId() {
        try(var f=new Fixture()) {
            f.publish();f.unknown();f.drain();assertNotNull(f.sendSuccess);
            f.generation.incrementAndGet();f.handler.getTopVoterMessageIds().put(TopVoter.Monthly,22L);
            var message=mock(Message.class);when(message.getIdLong()).thenReturn(10L);f.sendSuccess.accept(message);f.drain();
            assertEquals(22L,f.handler.getTopVoterMessageIds().get(TopVoter.Monthly));
            verify(f.data,never()).setTopVoterMessageId(TopVoter.Monthly,10L);
        }
    }
    @Test void duplicateUnknownCallbacksCoalesceAndFailedReplacementAllowsNextUpdate() {
        try(var f=new Fixture()) {
            f.publish();f.unknown();f.unknown();f.drain();
            verify(f.channel,times(1)).sendMessageEmbeds(any(MessageEmbed.class));
            f.sendFailure.accept(new IllegalStateException("synthetic send failure"));
            DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);
            verify(f.channel,times(2)).sendMessageEmbeds(any(MessageEmbed.class));
            var message=mock(Message.class);when(message.getIdLong()).thenReturn(12L);f.sendSuccess.accept(message);f.drain();
            assertEquals(12L,f.handler.getTopVoterMessageIds().get(TopVoter.Monthly));
            verify(f.data).setTopVoterMessageId(TopVoter.Monthly,12L);
        }
    }
    @Test void rejectedRecoveryAdmissionRetainsIdAndAllowsRetry() {
        try(var f=new Fixture()) {
            var timer=f.plugin.getTimer();
            doThrow(new RejectedExecutionException("synthetic")).when(timer).execute(any(Runnable.class));
            f.publish();f.unknown();assertNull(f.sent);assertEquals(9L,f.handler.getTopVoterMessageIds().get(TopVoter.Monthly));
            doAnswer(call->{f.worker.add(call.getArgument(0));return null;}).when(timer).execute(any(Runnable.class));
            f.unknown();f.drain();assertNotNull(f.sent);
        }
    }
    @Test void disabledRecoveryAndOtherErrorsPreserveExistingMessageId() {
        for(boolean disabled:List.of(true,false))try(var f=new Fixture()) {
            f.publish();
            if(disabled){when(f.config.isDiscordSRVTopVoterAutoRecoverMessageOnFailure()).thenReturn(false);f.unknown();}
            else f.editFailure.accept(new IllegalStateException("synthetic edit error"));
            f.drain();assertNull(f.sent);assertEquals(9L,f.handler.getTopVoterMessageIds().get(TopVoter.Monthly));
            verify(f.data,never()).setTopVoterMessageId(any(),anyLong());
        }
    }
    @Test void newMessageModeKeepsItsNonpersistentIdAndDropsLateGenerationSuccess() {
        try(var f=new Fixture()) {
            when(f.config.isDiscordSRVTopVoterNewMessageOnUpdate(any())).thenReturn(true);
            DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);
            var message=mock(Message.class);when(message.getIdLong()).thenReturn(12L);f.sendSuccess.accept(message);f.drain();
            assertEquals(12L,f.handler.getTopVoterMessageIds().get(TopVoter.Monthly));
            verify(f.data,never()).setTopVoterMessageId(any(),anyLong());
            DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);f.generation.incrementAndGet();
            when(message.getIdLong()).thenReturn(13L);f.sendSuccess.accept(message);f.drain();
            assertEquals(12L,f.handler.getTopVoterMessageIds().get(TopVoter.Monthly));
        }
    }

    @Test void recoveryReprojectsAfterBlockingIdPersistenceAndDropsRetiredGeneration() {
        for(boolean retire:List.of(false,true))try(var f=new Fixture()) {
            doAnswer(call->{
                if(retire)f.generation.incrementAndGet();
                else {var now=new LinkedHashMap<TopVoterPlayer,Integer>();now.put(f.first,100);now.putAll(f.board);f.board.clear();f.board.putAll(now);}
                return null;
            }).when(f.data).setTopVoterMessageId(TopVoter.Monthly,0L);
            f.publish();f.unknown();f.drain();
            if(retire)assertNull(f.sent,"No outgoing request after retirement during persistence");
            else assertTrue(f.sent.getDescription().contains("SyntheticFirst"),"Projection must follow persistence");
        }
    }
    @Test void ordinaryPendingSendsCoalesceAndOldCallbackCannotOverwriteSuccessor() {
        try(var f=new Fixture()) {
            when(f.config.isDiscordSRVTopVoterNewMessageOnUpdate(any())).thenReturn(true);
            DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);var old=f.sendSuccess;
            DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);
            verify(f.channel,times(1)).sendMessageEmbeds(any(MessageEmbed.class));
            var first=mock(Message.class);when(first.getIdLong()).thenReturn(12L);old.accept(first);f.drain();
            verify(f.channel,times(2)).sendMessageEmbeds(any(MessageEmbed.class));
            var next=mock(Message.class);when(next.getIdLong()).thenReturn(13L);f.sendSuccess.accept(next);f.drain();
            old.accept(first);f.drain();assertEquals(13L,f.handler.getTopVoterMessageIds().get(TopVoter.Monthly));
        }
    }

    @Test void lateEditFailureCannotRecoverOverSuccessorEdit() {
        try(var f=new Fixture()) {
            f.publish();var old=f.editFailure;f.publish();
            var error=mock(ErrorResponseException.class);when(error.getErrorResponse()).thenReturn(ErrorResponse.UNKNOWN_MESSAGE);
            old.accept(error);f.drain();assertNull(f.sent);
            f.unknown();f.drain();assertNotNull(f.sent);
        }
    }
    @Test void initialPersistentSendCoalescesAndDuplicateSuccessCannotOverwriteLaterId() {
        try(var f=new Fixture()) {
            f.handler.getTopVoterMessageIds().put(TopVoter.Monthly,0L);
            DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);var old=f.sendSuccess;
            DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);
            verify(f.channel,times(1)).sendMessageEmbeds(any(MessageEmbed.class));
            var message=mock(Message.class);when(message.getIdLong()).thenReturn(12L);
            old.accept(message);old.accept(message);f.drain();
            verify(f.data,times(1)).setTopVoterMessageId(TopVoter.Monthly,12L);
            f.unknown();f.drain();when(message.getIdLong()).thenReturn(13L);f.sendSuccess.accept(message);f.drain();
            when(message.getIdLong()).thenReturn(12L);old.accept(message);f.drain();
            assertEquals(13L,f.handler.getTopVoterMessageIds().get(TopVoter.Monthly));
        }
    }
    @Test void persistenceFailureReleasesSendAndRecoveryForRetry() {
        for(boolean recovery:List.of(false,true))try(var f=new Fixture()) {
            if(recovery) {
                doThrow(new IllegalStateException("synthetic persistence failure")).when(f.data).setTopVoterMessageId(TopVoter.Monthly,0L);
                f.publish();f.unknown();f.drain();assertNull(f.sent);
                doNothing().when(f.data).setTopVoterMessageId(TopVoter.Monthly,0L);
                f.publish();f.unknown();f.drain();assertNotNull(f.sent);
            } else {
                f.handler.getTopVoterMessageIds().put(TopVoter.Monthly,0L);
                DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);
                var message=mock(Message.class);when(message.getIdLong()).thenReturn(12L);
                doThrow(new IllegalStateException("synthetic persistence failure")).when(f.data).setTopVoterMessageId(TopVoter.Monthly,12L);
                f.sendSuccess.accept(message);
                assertDoesNotThrow(f::drain);
                verify(f.logger).warning(contains("storage"));
                assertEquals(12L,f.handler.getTopVoterMessageIds().get(TopVoter.Monthly));
                doAnswer(call->{f.storedId.set(12L);return null;}).when(f.data).setTopVoterMessageId(TopVoter.Monthly,12L);
                DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);
                f.editSuccess.accept(message);f.drain();
                assertEquals(12L,f.storedId.get());
                verify(f.channel,times(1)).sendMessageEmbeds(any(MessageEmbed.class));
                verify(f.channel).editMessageEmbedsById(eq(12L),any(MessageEmbed.class));
            }
        }
    }
    @Test void retirementWhileSuccessWaitsForDataMonitorPreventsPersistence() throws Exception {
        try(var f=new Fixture()) {
            f.handler.getTopVoterMessageIds().put(TopVoter.Monthly,0L);
            DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);
            var message=mock(Message.class);when(message.getIdLong()).thenReturn(12L);f.sendSuccess.accept(message);
            var action=f.worker.remove();var started=new CountDownLatch(1);var failure=new AtomicReference<Throwable>();
            Thread callback=new Thread(()->{started.countDown();try{action.run();}catch(Throwable error){failure.set(error);}});
            synchronized(f.data) {
                callback.start();assertTrue(started.await(5,TimeUnit.SECONDS));
                long end=System.nanoTime()+TimeUnit.SECONDS.toNanos(5);
                while(callback.getState()!=Thread.State.BLOCKED && callback.isAlive() && System.nanoTime()<end)Thread.onSpinWait();
                assertEquals(Thread.State.BLOCKED,callback.getState());f.generation.incrementAndGet();
            }
            callback.join(5000);assertFalse(callback.isAlive());assertNull(failure.get());
            verify(f.data,never()).setTopVoterMessageId(any(),anyLong());
            assertEquals(0L,f.handler.getTopVoterMessageIds().get(TopVoter.Monthly));
        }
    }
    @Test void rejectedSuccessDispatchReleasesOnlyItsOwnPublication() {
        try(var f=new Fixture()) {
            when(f.config.isDiscordSRVTopVoterNewMessageOnUpdate(any())).thenReturn(true);
            DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);var old=f.sendSuccess;
            var timer=f.plugin.getTimer();
            doThrow(new RejectedExecutionException("synthetic")).when(timer).execute(any(Runnable.class));
            var message=mock(Message.class);when(message.getIdLong()).thenReturn(12L);old.accept(message);
            doAnswer(call->{f.worker.add(call.getArgument(0));return null;}).when(timer).execute(any(Runnable.class));
            DiscordHandlerAccess.publish(f.handler,TopVoter.Monthly,f.board);var next=f.sendSuccess;
            old.accept(message);when(message.getIdLong()).thenReturn(13L);next.accept(message);f.drain();
            assertEquals(13L,f.handler.getTopVoterMessageIds().get(TopVoter.Monthly));
        }
    }

}
