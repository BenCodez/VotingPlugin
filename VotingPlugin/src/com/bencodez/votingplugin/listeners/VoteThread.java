package com.bencodez.votingplugin.listeners;

/**
 * The Class Thread.
 */
public class VoteThread {

	/**
	 * The Class ReadThread.
	 */
	public class ReadThread extends java.lang.Thread {

		@Override
		public void run() {
			while (!thread.isInterrupted()) {
				try {
					sleep(50);
				} catch (InterruptedException e) {
					e.printStackTrace();
					System.exit(0);
				}
			}
		}

		/**
		 * Run.
		 *
		 * @param run the run
		 */
		public void run(Runnable run) {
			synchronized (VoteThread.getInstance()) {
				run.run();
			}
		}
	}

	/** The instance. */
	static VoteThread instance = new VoteThread();

	/**
	 * Gets the single instance of Thread.
	 *
	 * @return single instance of Thread
	 */
	public static VoteThread getInstance() {
		return instance;
	}
	/** The thread. */
	private ReadThread thread;

	/**
	 * Instantiates a new thread.
	 */
	private VoteThread() {
	}

	/**
	 * @return the thread
	 */
	public ReadThread getThread() {
		if (thread == null || !thread.isAlive()) {
			loadThread();
		}
		return thread;
	}

	/**
	 * Load thread.
	 */
	public void loadThread() {
		thread = new ReadThread();
		thread.start();
	}

	/**
	 * Run.
	 *
	 * @param run the run
	 */
	public void run(Runnable run) {
		getThread().run(run);
	}
}
