package com.Ben12345rocks.VotingPlugin.Util.Updater;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;
import java.net.ProtocolException;
import java.net.URL;

import org.bukkit.plugin.java.JavaPlugin;

// TODO: Auto-generated Javadoc
/**
 * The Class Updater.
 */
public class Updater {

	/**
	 * The Enum UpdateResult.
	 */
	public enum UpdateResult {

		/** The bad resourceid. */
		BAD_RESOURCEID,

		/** The disabled. */
		DISABLED,

		/** The fail noversion. */
		FAIL_NOVERSION,

		/** The fail spigot. */
		FAIL_SPIGOT,

		/** The no update. */
		NO_UPDATE,

		/** The update available. */
		UPDATE_AVAILABLE
	}

	/** The api key. */
	private final String API_KEY = "98BE0FE67F88AB82B4C197FAF1DC3B69206EFDCC4D3B80FC83A00037510B99B4";

	/** The connection. */
	private HttpURLConnection connection;

	/** The host. */
	private final String HOST = "http://www.spigotmc.org";

	/** The old version. */
	private String oldVersion;

	/** The plugin. */
	private JavaPlugin plugin;

	/** The query. */
	private final String QUERY = "/api/general.php";

	/** The request method. */
	private final String REQUEST_METHOD = "POST";

	/** The resource id. */
	private String RESOURCE_ID = "";

	/** The result. */
	private Updater.UpdateResult result = Updater.UpdateResult.DISABLED;

	/** The version. */
	private String version;

	/** The write string. */
	private String WRITE_STRING;

	/**
	 * Instantiates a new updater.
	 *
	 * @param plugin
	 *            the plugin
	 * @param resourceId
	 *            the resource id
	 * @param disabled
	 *            the disabled
	 */
	public Updater(JavaPlugin plugin, Integer resourceId, boolean disabled) {
		RESOURCE_ID = resourceId + "";
		this.plugin = plugin;
		oldVersion = this.plugin.getDescription().getVersion();

		if (disabled) {
			result = UpdateResult.DISABLED;
			return;
		}

		try {
			connection = (HttpURLConnection) new URL(HOST + QUERY)
					.openConnection();
		} catch (IOException e) {
			result = UpdateResult.FAIL_SPIGOT;
			return;
		}

		WRITE_STRING = "key=" + API_KEY + "&resource=" + RESOURCE_ID;
		run();
	}

	/**
	 * Gets the result.
	 *
	 * @return the result
	 */
	public UpdateResult getResult() {
		return result;
	}

	/**
	 * Gets the version.
	 *
	 * @return the version
	 */
	public String getVersion() {
		return version;
	}

	/**
	 * Run.
	 */
	private void run() {
		connection.setDoOutput(true);
		try {
			connection.setRequestMethod(REQUEST_METHOD);
			connection.getOutputStream().write(WRITE_STRING.getBytes("UTF-8"));
		} catch (ProtocolException e1) {
			result = UpdateResult.FAIL_SPIGOT;
		} catch (UnsupportedEncodingException e) {
			result = UpdateResult.FAIL_SPIGOT;
		} catch (IOException e) {
			result = UpdateResult.FAIL_SPIGOT;
		}
		String version;
		try {
			version = new BufferedReader(new InputStreamReader(
					connection.getInputStream())).readLine();
		} catch (Exception e) {
			result = UpdateResult.BAD_RESOURCEID;
			return;
		}
		if (version.length() <= 7) {
			this.version = version;
			version.replace("[^A-Za-z]", "").replace("|", "");
			versionCheck();
			return;
		}
		result = UpdateResult.BAD_RESOURCEID;
	}

	/**
	 * Should update.
	 *
	 * @param localVersion
	 *            the local version
	 * @param remoteVersion
	 *            the remote version
	 * @return true, if successful
	 */
	public boolean shouldUpdate(String localVersion, String remoteVersion) {
		return !localVersion.equalsIgnoreCase(remoteVersion);
	}

	/**
	 * Version check.
	 */
	private void versionCheck() {
		if (shouldUpdate(oldVersion, version)) {
			result = UpdateResult.UPDATE_AVAILABLE;
		} else {
			result = UpdateResult.NO_UPDATE;
		}
	}

}
