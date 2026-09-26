package com.bencodez.votingplugin.data;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.EnumMap;
import java.util.Map;
import java.util.Properties;

import com.bencodez.advancedcore.api.time.TimeChangeTransition;
import com.bencodez.advancedcore.api.time.TimeType;
import com.bencodez.votingplugin.data.ServerData.TimeChangeRewardState;
import com.bencodez.votingplugin.data.ServerData.TimeChangeUserProgress;
import com.bencodez.votingplugin.util.DurableFiles;

/**
 * Compact durable state for the per-user portion of period transitions.
 *
 * <p>The general server YAML can contain large top-voter archives. Keeping the
 * hot cursor and one in-flight streak receipt here makes each user commit
 * constant-sized while retaining the existing write-before-next-user ordering.</p>
 */
final class TimeChangeUserCheckpointStore {
	private static final String FILE_NAME = "TimeChangeUserCheckpoint.properties";

	private final Path file;
	private final Map<TimeType, State> states = new EnumMap<>(TimeType.class);

	TimeChangeUserCheckpointStore(Path dataDirectory) {
		file = dataDirectory == null ? null : dataDirectory.resolve(FILE_NAME);
		if (file != null) load();
	}

	boolean isFileBacked() {
		return file != null;
	}

	synchronized void begin(TimeChangeTransition transition, String legacyCursor,
			TimeChangeUserProgress legacyUser, TimeChangeRewardState legacyRewardState) {
		State current = states.get(transition.getType());
		if (current != null && transition.getId().equals(current.id)) return;
		State replacement = new State(transition.getId());
		replacement.cursor = legacyCursor == null ? "" : legacyCursor;
		if (legacyUser != null && legacyUser.uuid() != null && !legacyUser.uuid().isEmpty()) {
			replacement.userUuid = legacyUser.uuid();
			replacement.streakTarget = legacyUser.streakTarget();
			replacement.rewardRequired = legacyUser.rewardRequired();
			replacement.rewardState = legacyRewardState == null
					? (legacyUser.rewardComplete() ? TimeChangeRewardState.COMPLETE : TimeChangeRewardState.UNCLAIMED)
					: legacyRewardState;
		}
		persist(transition.getType(), replacement);
	}

	synchronized String cursor(TimeChangeTransition transition) {
		State state = state(transition);
		return state == null ? "" : state.cursor;
	}

	synchronized void completeUser(TimeChangeTransition transition, String uuid) {
		State replacement = copy(requiredState(transition));
		replacement.cursor = uuid;
		replacement.clearUser();
		persist(transition.getType(), replacement);
	}

	synchronized TimeChangeUserProgress prepareStreak(TimeChangeTransition transition, String uuid,
			int streakTarget, boolean rewardRequired) {
		State state = requiredState(transition);
		if (!uuid.equals(state.userUuid)) {
			State replacement = copy(state);
			replacement.userUuid = uuid;
			replacement.streakTarget = streakTarget;
			replacement.rewardRequired = rewardRequired;
			replacement.rewardState = TimeChangeRewardState.UNCLAIMED;
			persist(transition.getType(), replacement);
			state = requiredState(transition);
		}
		return new TimeChangeUserProgress(uuid, state.streakTarget, state.rewardRequired,
				state.rewardState == TimeChangeRewardState.COMPLETE);
	}

	synchronized TimeChangeRewardState rewardState(TimeChangeTransition transition, String uuid) {
		State state = requiredState(transition);
		if (!uuid.equals(state.userUuid)) throw new IllegalStateException("Time change recovery user does not match");
		return state.rewardState;
	}

	synchronized void setRewardState(TimeChangeTransition transition, String uuid, TimeChangeRewardState value) {
		State state = requiredState(transition);
		if (!uuid.equals(state.userUuid)) throw new IllegalStateException("Time change recovery user does not match");
		State replacement = copy(state);
		replacement.rewardState = value;
		persist(transition.getType(), replacement);
	}

	private State state(TimeChangeTransition transition) {
		State state = states.get(transition.getType());
		return state != null && transition.getId().equals(state.id) ? state : null;
	}

	private State requiredState(TimeChangeTransition transition) {
		State state = state(transition);
		if (state == null) throw new IllegalStateException("Time change recovery transition does not match");
		return state;
	}

	private void persist(TimeType type, State replacement) {
		State previous = states.put(type, replacement);
		if (file == null) return;
		Path temporary = null;
		try {
			Path parent = file.toAbsolutePath().normalize().getParent();
			Files.createDirectories(parent);
			temporary = Files.createTempFile(parent, FILE_NAME, ".tmp");
			Properties properties = encode();
			ByteArrayOutputStream bytes = new ByteArrayOutputStream();
			properties.store(bytes, "VotingPlugin time-change user checkpoint");
			Files.write(temporary, bytes.toByteArray(), StandardOpenOption.TRUNCATE_EXISTING,
					StandardOpenOption.WRITE);
			DurableFiles.publishStagedFile(temporary, file);
		} catch (DurableFiles.PublishedException published) {
			throw new IllegalStateException("Checkpoint was published but directory durability failed", published);
		} catch (IOException failure) {
			if (previous == null) states.remove(type); else states.put(type, previous);
			throw new IllegalStateException("Unable to persist time-change user checkpoint", failure);
		} finally {
			if (temporary != null) {
				try { Files.deleteIfExists(temporary); }
				catch (IOException ignored) { }
			}
		}
	}

	private Properties encode() {
		Properties properties = new Properties();
		for (Map.Entry<TimeType, State> entry : states.entrySet()) {
			String prefix = entry.getKey().name() + ".";
			State state = entry.getValue();
			properties.setProperty(prefix + "id", state.id);
			properties.setProperty(prefix + "cursor", state.cursor);
			if (!state.userUuid.isEmpty()) {
				properties.setProperty(prefix + "user.uuid", state.userUuid);
				properties.setProperty(prefix + "user.streakTarget", Integer.toString(state.streakTarget));
				properties.setProperty(prefix + "user.rewardRequired", Boolean.toString(state.rewardRequired));
				properties.setProperty(prefix + "user.rewardState", state.rewardState.name());
			}
		}
		return properties;
	}

	private void load() {
		if (!Files.isRegularFile(file)) return;
		Properties properties = new Properties();
		try (InputStream input = Files.newInputStream(file)) {
			properties.load(input);
			for (TimeType type : TimeType.values()) {
				String prefix = type.name() + ".";
				String id = properties.getProperty(prefix + "id", "");
				if (id.isEmpty()) continue;
				State state = new State(id);
				state.cursor = properties.getProperty(prefix + "cursor", "");
				state.userUuid = properties.getProperty(prefix + "user.uuid", "");
				state.streakTarget = Integer.parseInt(properties.getProperty(prefix + "user.streakTarget", "0"));
				state.rewardRequired = Boolean.parseBoolean(properties.getProperty(prefix + "user.rewardRequired", "false"));
				state.rewardState = TimeChangeRewardState.valueOf(
						properties.getProperty(prefix + "user.rewardState", TimeChangeRewardState.UNCLAIMED.name()));
				states.put(type, state);
			}
		} catch (IOException | RuntimeException failure) {
			throw new IllegalStateException("Unable to read time-change user checkpoint", failure);
		}
	}

	private static State copy(State source) {
		State copy = new State(source.id);
		copy.cursor = source.cursor;
		copy.userUuid = source.userUuid;
		copy.streakTarget = source.streakTarget;
		copy.rewardRequired = source.rewardRequired;
		copy.rewardState = source.rewardState;
		return copy;
	}

	private static final class State {
		private final String id;
		private String cursor = "";
		private String userUuid = "";
		private int streakTarget;
		private boolean rewardRequired;
		private TimeChangeRewardState rewardState = TimeChangeRewardState.UNCLAIMED;

		private State(String id) { this.id = id; }
		private void clearUser() {
			userUuid = "";
			streakTarget = 0;
			rewardRequired = false;
			rewardState = TimeChangeRewardState.UNCLAIMED;
		}
	}
}
