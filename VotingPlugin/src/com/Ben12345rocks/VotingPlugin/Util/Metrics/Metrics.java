package com.Ben12345rocks.VotingPlugin.Util.Metrics;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.net.Proxy;
import java.net.URL;
import java.net.URLConnection;
import java.net.URLEncoder;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.UUID;

import org.bukkit.Bukkit;
import org.bukkit.configuration.InvalidConfigurationException;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.plugin.Plugin;
import org.bukkit.plugin.PluginDescriptionFile;

import com.Ben12345rocks.VotingPlugin.Main;

// TODO: Auto-generated Javadoc
/**
 * The Class Metrics.
 */
public class Metrics {

	/**
	 * The Class Graph.
	 */
	public static class Graph {

		/** The name. */
		private final String name;

		/** The plotters. */
		private final Set<Plotter> plotters = new LinkedHashSet<Plotter>();

		/**
		 * Instantiates a new graph.
		 *
		 * @param name
		 *            the name
		 */
		private Graph(final String name) {
			this.name = name;
		}

		/**
		 * Adds the plotter.
		 *
		 * @param plotter
		 *            the plotter
		 */
		public void addPlotter(final Plotter plotter) {
			plotters.add(plotter);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		@Override
		public boolean equals(final Object object) {
			if (!(object instanceof Graph)) {
				return false;
			}

			final Graph graph = (Graph) object;
			return graph.name.equals(name);
		}

		/**
		 * Gets the name.
		 *
		 * @return the name
		 */
		public String getName() {
			return name;
		}

		/**
		 * Gets the plotters.
		 *
		 * @return the plotters
		 */
		public Set<Plotter> getPlotters() {
			return Collections.unmodifiableSet(plotters);
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			return name.hashCode();
		}

		/**
		 * Removes the plotter.
		 *
		 * @param plotter
		 *            the plotter
		 */
		public void removePlotter(final Plotter plotter) {
			plotters.remove(plotter);
		}

	}

	/**
	 * The Class Plotter.
	 */
	public static abstract class Plotter {

		/** The name. */
		private final String name;

		/**
		 * Instantiates a new plotter.
		 */
		public Plotter() {
			this("Default");
		}

		/**
		 * Instantiates a new plotter.
		 *
		 * @param name
		 *            the name
		 */
		public Plotter(final String name) {
			this.name = name;
		}

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.lang.Object#equals(java.lang.Object)
		 */
		@Override
		public boolean equals(final Object object) {
			if (!(object instanceof Plotter)) {
				return false;
			}

			final Plotter plotter = (Plotter) object;
			return plotter.name.equals(name)
					&& (plotter.getValue() == getValue());
		}

		/**
		 * Gets the column name.
		 *
		 * @return the column name
		 */
		public String getColumnName() {
			return name;
		}

		/**
		 * Gets the value.
		 *
		 * @return the value
		 */
		public abstract int getValue();

		/*
		 * (non-Javadoc)
		 * 
		 * @see java.lang.Object#hashCode()
		 */
		@Override
		public int hashCode() {
			return getColumnName().hashCode() + getValue();
		}

		/**
		 * Reset.
		 */
		public void reset() {
		}

	}

	/** The Constant BASE_URL. */
	private static final String BASE_URL = "http://mcstats.org";

	/** The Constant CONFIG_FILE. */
	private static final String CONFIG_FILE = "plugins/PluginMetrics/config.yml";

	/** The Constant CUSTOM_DATA_SEPARATOR. */
	private static final String CUSTOM_DATA_SEPARATOR = "~~";

	/** The Constant PING_INTERVAL. */
	private static final int PING_INTERVAL = 10;

	/** The Constant REPORT_URL. */
	private static final String REPORT_URL = "/report/%s";

	/** The Constant REVISION. */
	private final static int REVISION = 5;

	/**
	 * Encode.
	 *
	 * @param text
	 *            the text
	 * @return the string
	 * @throws UnsupportedEncodingException
	 *             the unsupported encoding exception
	 */
	private static String encode(final String text)
			throws UnsupportedEncodingException {
		return URLEncoder.encode(text, "UTF-8");
	}

	/**
	 * Encode data pair.
	 *
	 * @param buffer
	 *            the buffer
	 * @param key
	 *            the key
	 * @param value
	 *            the value
	 * @throws UnsupportedEncodingException
	 *             the unsupported encoding exception
	 */
	private static void encodeDataPair(final StringBuilder buffer,
			final String key, final String value)
			throws UnsupportedEncodingException {
		buffer.append('&').append(encode(key)).append('=')
				.append(encode(value));
	}

	/** The configuration. */
	private final YamlConfiguration configuration;

	/** The configuration file. */
	private final File configurationFile;

	/** The default graph. */
	private final Graph defaultGraph = new Graph("Default");

	/** The graphs. */
	private final Set<Graph> graphs = Collections
			.synchronizedSet(new HashSet<Graph>());

	/** The guid. */
	private final String guid;

	/** The opt out lock. */
	private final Object optOutLock = new Object();

	/** The plugin. */
	private final Plugin plugin;

	/** The task id. */
	private volatile int taskId = -1;

	/**
	 * Instantiates a new metrics.
	 *
	 * @param plugin
	 *            the plugin
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	public Metrics(final Plugin plugin) throws IOException {
		if (plugin == null) {
			throw new IllegalArgumentException("Plugin cannot be null");
		}

		this.plugin = plugin;

		// load the config
		configurationFile = new File(CONFIG_FILE);
		configuration = YamlConfiguration.loadConfiguration(configurationFile);

		// add some defaults
		configuration.addDefault("opt-out", false);
		configuration.addDefault("guid", UUID.randomUUID().toString());

		// Do we need to create the file?
		if (configuration.get("guid", null) == null) {
			configuration.options().header("http://mcstats.org")
					.copyDefaults(true);
			configuration.save(configurationFile);
		}

		// Load the guid then
		guid = configuration.getString("guid");
	}

	/**
	 * Adds the custom data.
	 *
	 * @param plotter
	 *            the plotter
	 */
	public void addCustomData(final Plotter plotter) {
		if (plotter == null) {
			throw new IllegalArgumentException("Plotter cannot be null");
		}

		// Add the plotter to the graph o/
		defaultGraph.addPlotter(plotter);

		// Ensure the default graph is included in the submitted graphs
		graphs.add(defaultGraph);
	}

	/**
	 * Creates the graph.
	 *
	 * @param name
	 *            the name
	 * @return the graph
	 */
	public Graph createGraph(final String name) {
		if (name == null) {
			throw new IllegalArgumentException("Graph name cannot be null");
		}

		// Construct the graph object
		final Graph graph = new Graph(name);

		// Now we can add our graph
		graphs.add(graph);

		// and return back
		return graph;
	}

	/**
	 * Disable.
	 *
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	public void disable() throws IOException {
		// This has to be synchronized or it can collide with the check in the
		// task.
		synchronized (optOutLock) {
			// Check if the server owner has already set opt-out, if not, set
			// it.
			if (!isOptOut()) {
				configuration.set("opt-out", true);
				configuration.save(configurationFile);
			}

			// Disable Task, if it is running
			if (taskId > 0) {
				plugin.getServer().getScheduler().cancelTask(taskId);
				taskId = -1;
			}
		}
	}

	/**
	 * Enable.
	 *
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	public void enable() throws IOException {
		// This has to be synchronized or it can collide with the check in the
		// task.
		synchronized (optOutLock) {
			// Check if the server owner has already set opt-out, if not, set
			// it.
			if (isOptOut()) {
				configuration.set("opt-out", false);
				configuration.save(configurationFile);
			}

			// Enable Task, if it is not running
			if (taskId < 0) {
				start();
			}
		}
	}

	/**
	 * Checks if is mineshafter present.
	 *
	 * @return true, if is mineshafter present
	 */
	private boolean isMineshafterPresent() {
		try {
			Class.forName("mineshafter.MineServer");
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	/**
	 * Checks if is opt out.
	 *
	 * @return true, if is opt out
	 */
	public boolean isOptOut() {
		synchronized (optOutLock) {
			try {
				// Reload the metrics file
				configuration.load(CONFIG_FILE);
			} catch (IOException ex) {

				Main.plugin.debug("[Metrics] " + ex.getMessage());

				return true;
			} catch (InvalidConfigurationException ex) {
				Main.plugin.debug("[Metrics] " + ex.getMessage());

				return true;
			}
			return configuration.getBoolean("opt-out", false);
		}
	}

	/**
	 * Post plugin.
	 *
	 * @param isPing
	 *            the is ping
	 * @throws IOException
	 *             Signals that an I/O exception has occurred.
	 */
	private void postPlugin(final boolean isPing) throws IOException {
		// The plugin's description file containg all of the plugin data such as
		// name, version, author, etc
		final PluginDescriptionFile description = plugin.getDescription();

		// Construct the post data
		final StringBuilder data = new StringBuilder();
		data.append(encode("guid")).append('=').append(encode(guid));
		encodeDataPair(data, "version", description.getVersion());
		encodeDataPair(data, "server", Bukkit.getVersion());
		encodeDataPair(data, "players",
				Integer.toString(Bukkit.getServer().getOnlinePlayers().size()));
		encodeDataPair(data, "revision", String.valueOf(REVISION));

		// If we're pinging, append it
		if (isPing) {
			encodeDataPair(data, "ping", "true");
		}

		// Acquire a lock on the graphs, which lets us make the assumption we
		// also lock everything
		// inside of the graph (e.g plotters)
		synchronized (graphs) {
			final Iterator<Graph> iter = graphs.iterator();

			while (iter.hasNext()) {
				final Graph graph = iter.next();

				for (Plotter plotter : graph.getPlotters()) {
					// The key name to send to the metrics server
					// The format is C-GRAPHNAME-PLOTTERNAME where separator -
					// is defined at the top
					// Legacy (R4) submitters use the format Custom%s, or
					// CustomPLOTTERNAME
					final String key = String.format("C%s%s%s%s",
							CUSTOM_DATA_SEPARATOR, graph.getName(),
							CUSTOM_DATA_SEPARATOR, plotter.getColumnName());

					// The value to send, which for the foreseeable future is
					// just the string
					// value of plotter.getValue()
					final String value = Integer.toString(plotter.getValue());

					// Add it to the http post data :)
					encodeDataPair(data, key, value);
				}
			}
		}

		// Create the url
		URL url = new URL(BASE_URL
				+ String.format(REPORT_URL, encode(plugin.getDescription()
						.getName())));

		// Connect to the website
		URLConnection connection;

		// Mineshafter creates a socks proxy, so we can safely bypass it
		// It does not reroute POST requests so we need to go around it
		if (isMineshafterPresent()) {
			connection = url.openConnection(Proxy.NO_PROXY);
		} else {
			connection = url.openConnection();
		}

		connection.setDoOutput(true);

		// Write the data
		final OutputStreamWriter writer = new OutputStreamWriter(
				connection.getOutputStream());
		writer.write(data.toString());
		writer.flush();

		// Now read the response
		final BufferedReader reader = new BufferedReader(new InputStreamReader(
				connection.getInputStream()));
		final String response = reader.readLine();

		// close resources
		writer.close();
		reader.close();

		if ((response == null) || response.startsWith("ERR")) {
			throw new IOException(response); // Throw the exception
		} else {
			// Is this the first update this hour?
			if (response.contains("OK This is your first update this hour")) {
				synchronized (graphs) {
					final Iterator<Graph> iter = graphs.iterator();

					while (iter.hasNext()) {
						final Graph graph = iter.next();

						for (Plotter plotter : graph.getPlotters()) {
							plotter.reset();
						}
					}
				}
			}
		}
	}

	/**
	 * Start.
	 *
	 * @return true, if successful
	 */
	@SuppressWarnings("deprecation")
	public boolean start() {
		synchronized (optOutLock) {
			// Did we opt out?
			if (isOptOut()) {
				return false;
			}

			// Is metrics already running?
			if (taskId >= 0) {
				return true;
			}

			// Begin hitting the server with glorious data
			taskId = plugin.getServer().getScheduler()
					.scheduleAsyncRepeatingTask(plugin, new Runnable() {

						private boolean firstPost = true;

						@Override
						public void run() {
							try {
								// This has to be synchronized or it can collide
								// with the disable method.
								synchronized (optOutLock) {
									// Disable Task, if it is running and the
									// server owner decided to opt-out
									if (isOptOut() && (taskId > 0)) {
										plugin.getServer().getScheduler()
												.cancelTask(taskId);
										taskId = -1;
									}
								}

								// We use the inverse of firstPost because if it
								// is the first time we are posting,
								// it is not a interval ping, so it evaluates to
								// FALSE
								// Each time thereafter it will evaluate to
								// TRUE, i.e PING!
								postPlugin(!firstPost);

								// After the first post we set firstPost to
								// false
								// Each post thereafter will be a ping
								firstPost = false;
							} catch (IOException e) {

								Main.plugin.debug("[Metrics] " + e.getMessage());

							}
						}
					}, 0, PING_INTERVAL * 1200);

			return true;
		}
	}

}