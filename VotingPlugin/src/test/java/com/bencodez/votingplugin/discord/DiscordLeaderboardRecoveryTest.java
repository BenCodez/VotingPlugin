package com.bencodez.votingplugin.discord;

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
import github.scarsz.discordsrv.dependencies.jda.api.requests.ErrorResponse;
import github.scarsz.discordsrv.dependencies.jda.api.exceptions.ErrorResponseException;

class DiscordLeaderboardRecoveryTest {
    static final class Fixture implements AutoCloseable {
        final VotingPluginMain plugin=mock(VotingPluginMain.class);
        final Logger logger=mock(Logger.class);
        final Config config=mock(Config.class);final ServerData data=mock(ServerData.class);
        final TextChannel channel=mock(TextChannel.class);
        final MessageAction edit=mock(MessageAction.class),send=mock(MessageAction.class);
        final AtomicLong generation=new AtomicLong();final ArrayDeque<Runnable> worker=new ArrayDeque<>();
        final LinkedHashMap<TopVoterPlayer,Integer> board=new LinkedHashMap<>();
        final TopVoterPlayer first=new TopVoterPlayer(new UUID(0,1),"SyntheticFirst",1L);
        final TopVoterPlayer second=new TopVoterPlayer(new UUID(0,2),"SyntheticSecond",1L);
        Consumer<Throwable> editFailure,sendFailure;Consumer<Message> sendSuccess;MessageEmbed sent;
        final DiscordHandler handler;
        Fixture() {
            when(plugin.getConfigFile()).thenReturn(config);when(plugin.getServerData()).thenReturn(data);
            when(plugin.getLogger()).thenReturn(logger);when(plugin.isEnabled()).thenReturn(true);
            when(plugin.getTopVoterDisplayGeneration()).thenAnswer(call->generation.get());
            when(plugin.getTopVoterDisplay(any())).thenAnswer(call->new LinkedHashMap<>(board));
            when(config.getDiscordSRVTopVoterChannel(any())).thenReturn(1L);
            when(config.getDiscordSRVTopVoterTitle(any())).thenReturn("Synthetic board");
            when(config.isDiscordSRVTopVoterAutoRecoverMessageOnFailure()).thenReturn(true);
            when(data.getTopVoterMessageId(any())).thenReturn(9L);
            var timer=mock(ScheduledExecutorService.class);when(plugin.getTimer()).thenReturn(timer);
            doAnswer(call->{worker.add(call.getArgument(0));return null;}).when(timer).execute(any(Runnable.class));
            when(channel.editMessageEmbedsById(anyLong(),any(MessageEmbed.class))).thenReturn(edit);
            when(channel.sendMessageEmbeds(any(MessageEmbed.class))).thenAnswer(call->{sent=call.getArgument(0);return send;});
            doAnswer(call->{editFailure=call.getArgument(1);return null;}).when(edit).queue(any(),any());
            doAnswer(call->{sendSuccess=call.getArgument(0);sendFailure=call.getArgument(1);return null;}).when(send).queue(any(),any());
            board.put(second,90);handler=new DiscordHandler(plugin) {
                @Override TextChannel leaderboardChannel(long channelId) { assertEquals(1L,channelId);return channel; }
            };
        }
        void publish() {handler.publishTopVoterMessage(TopVoter.Monthly,new LinkedHashMap<>(board));assertNotNull(editFailure);}
        void unknown() {var error=mock(ErrorResponseException.class);when(error.getErrorResponse()).thenReturn(ErrorResponse.UNKNOWN_MESSAGE);editFailure.accept(error);}
        void drain() {int bound=20;while(!worker.isEmpty()){assertTrue(bound-->0);worker.remove().run();}}
        public void close(){}
    }
    @Test void failedSendStillPublishesOneExplicitCoalescedRefreshInEverySendMode() {
        for(int mode=0;mode<3;mode++)try(var f=new Fixture()) {
            if(mode==0)when(f.config.isDiscordSRVTopVoterNewMessageOnUpdate(any())).thenReturn(true);
            if(mode==1)f.handler.getTopVoterMessageIds().put(TopVoter.Monthly,0L);
            if(mode==2){f.publish();f.unknown();f.drain();}
            else f.handler.publishTopVoterMessage(TopVoter.Monthly,f.board);
            f.board.clear();f.board.put(f.first,100);
            f.handler.publishTopVoterMessage(TopVoter.Monthly,f.board);
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
                f.handler.publishTopVoterMessage(TopVoter.Monthly,f.board);
                f.sendFailure.accept(new IllegalStateException("synthetic-private-detail"));}
            else {f.publish();f.editFailure.accept(new IllegalStateException("synthetic-private-detail"));}
            verify(f.logger).warning(argThat((String text)->text.contains("Discord")&&!text.contains("synthetic-private-detail")));
        }
    }
    @Test void staleFailedSendCannotFlushAnOlderCoalescedRequest() {
        try(var f=new Fixture()) {
            when(f.config.isDiscordSRVTopVoterNewMessageOnUpdate(any())).thenReturn(true);
            f.handler.publishTopVoterMessage(TopVoter.Monthly,f.board);
            f.handler.publishTopVoterMessage(TopVoter.Monthly,f.board);
            f.generation.incrementAndGet();f.sendFailure.accept(new IllegalStateException("synthetic stale failure"));f.drain();
            verify(f.channel,times(1)).sendMessageEmbeds(any(MessageEmbed.class));verifyNoInteractions(f.logger);
        }
    }
    @Test void missingChannelBuildAndRecoveryFailuresHaveRedactedDiagnostics() {
        for(int mode=0;mode<3;mode++)try(var f=new Fixture()) {
            if(mode==0) {
                var absent=new DiscordHandler(f.plugin) {@Override TextChannel leaderboardChannel(long id){return null;}};
                absent.publishTopVoterMessage(TopVoter.Monthly,f.board);
            } else if(mode==1) {
                when(f.config.getDiscordSRVTopVoterTitle(any())).thenThrow(new IllegalStateException("synthetic-private-detail"));
                f.handler.publishTopVoterMessage(TopVoter.Monthly,f.board);
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
            f.handler.publishTopVoterMessage(TopVoter.Monthly,f.board);
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
            f.handler.publishTopVoterMessage(TopVoter.Monthly,f.board);
            var message=mock(Message.class);when(message.getIdLong()).thenReturn(12L);f.sendSuccess.accept(message);f.drain();
            assertEquals(12L,f.handler.getTopVoterMessageIds().get(TopVoter.Monthly));
            verify(f.data,never()).setTopVoterMessageId(any(),anyLong());
            f.handler.publishTopVoterMessage(TopVoter.Monthly,f.board);f.generation.incrementAndGet();
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
            f.handler.publishTopVoterMessage(TopVoter.Monthly,f.board);var old=f.sendSuccess;
            f.handler.publishTopVoterMessage(TopVoter.Monthly,f.board);
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
            f.handler.publishTopVoterMessage(TopVoter.Monthly,f.board);var old=f.sendSuccess;
            f.handler.publishTopVoterMessage(TopVoter.Monthly,f.board);
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
                f.handler.publishTopVoterMessage(TopVoter.Monthly,f.board);
                var message=mock(Message.class);when(message.getIdLong()).thenReturn(12L);
                doThrow(new IllegalStateException("synthetic persistence failure")).when(f.data).setTopVoterMessageId(TopVoter.Monthly,12L);
                f.sendSuccess.accept(message);
                assertDoesNotThrow(f::drain);
                verify(f.logger).warning(contains("storing a message ID"));
                assertEquals(0L,f.handler.getTopVoterMessageIds().get(TopVoter.Monthly));
                f.handler.publishTopVoterMessage(TopVoter.Monthly,f.board);
                verify(f.channel,times(2)).sendMessageEmbeds(any(MessageEmbed.class));
            }
        }
    }
    @Test void retirementWhileSuccessWaitsForDataMonitorPreventsPersistence() throws Exception {
        try(var f=new Fixture()) {
            f.handler.getTopVoterMessageIds().put(TopVoter.Monthly,0L);
            f.handler.publishTopVoterMessage(TopVoter.Monthly,f.board);
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
            f.handler.publishTopVoterMessage(TopVoter.Monthly,f.board);var old=f.sendSuccess;
            var timer=f.plugin.getTimer();
            doThrow(new RejectedExecutionException("synthetic")).when(timer).execute(any(Runnable.class));
            var message=mock(Message.class);when(message.getIdLong()).thenReturn(12L);old.accept(message);
            doAnswer(call->{f.worker.add(call.getArgument(0));return null;}).when(timer).execute(any(Runnable.class));
            f.handler.publishTopVoterMessage(TopVoter.Monthly,f.board);var next=f.sendSuccess;
            old.accept(message);when(message.getIdLong()).thenReturn(13L);next.accept(message);f.drain();
            assertEquals(13L,f.handler.getTopVoterMessageIds().get(TopVoter.Monthly));
        }
    }

}
