package com.Ben12345rocks.VotingPlugin.Report;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;

import com.Ben12345rocks.VotingPlugin.Main;
import com.Ben12345rocks.VotingPlugin.Files.Files;

// TODO: Auto-generated Javadoc
/**
 * The Class Report.
 */
public class Report {

	/** The instance. */
	static Report instance = new Report();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Adds the to zip.
	 *
	 * @param directoryToZip the directory to zip
	 * @param file the file
	 * @param zos the zos
	 * @throws FileNotFoundException the file not found exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	public static void addToZip(File directoryToZip, File file,
			ZipOutputStream zos) throws FileNotFoundException, IOException {

		FileInputStream fis = new FileInputStream(file);

		String zipFilePath = file.getPath();

		plugin.debug("Writing '" + zipFilePath + "' to zip file");

		ZipEntry zipEntry = new ZipEntry(zipFilePath);
		zos.putNextEntry(zipEntry);

		byte[] bytes = new byte[1024];
		int length;
		while ((length = fis.read(bytes)) >= 0) {
			zos.write(bytes, 0, length);
		}

		zos.closeEntry();
		fis.close();
	}

	/**
	 * Gets the single instance of Report.
	 *
	 * @return single instance of Report
	 */
	public static Report getInstance() {
		return instance;
	}

	/** The data. */
	FileConfiguration data;

	/** The d file. */
	File dFile;

	/**
	 * Instantiates a new report.
	 */
	private Report() {
	}

	/**
	 * Instantiates a new report.
	 *
	 * @param plugin the plugin
	 */
	public Report(Main plugin) {
		Report.plugin = plugin;
	}

	/**
	 * Creates the.
	 */
	public void create() {
		File directoryToZip = plugin.getDataFolder();

		List<File> fileList = new ArrayList<File>();
		try {
			plugin.getLogger().info(
					"---Getting references to all files in: "
							+ directoryToZip.getCanonicalPath());
		} catch (IOException e) {
			e.printStackTrace();
		}
		getAllFiles(directoryToZip, fileList);
		plugin.getLogger().info("---Creating zip file");
		writeZipFile(directoryToZip, fileList);
		plugin.getLogger().info("---Done");
	}

	/**
	 * Gets the all files.
	 *
	 * @param dir the dir
	 * @param fileList the file list
	 */
	public void getAllFiles(File dir, List<File> fileList) {
		try {
			File[] files = dir.listFiles();
			for (File file : files) {
				fileList.add(file);
				if (file.isDirectory()) {

					plugin.debug("directory:" + file.getCanonicalPath());

					getAllFiles(file, fileList);
				} else {

					plugin.debug("file:" + file.getCanonicalPath());

				}
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * Gets the data.
	 *
	 * @return the data
	 */
	public FileConfiguration getData() {
		return data;
	}

	/**
	 * Reload data.
	 */
	public void reloadData() {
		data = YamlConfiguration.loadConfiguration(dFile);
	}

	/**
	 * Save data.
	 */
	public void saveData() {
		Files.getInstance().editFile(dFile, data);
	}

	/**
	 * Write zip file.
	 *
	 * @param directoryToZip the directory to zip
	 * @param fileList the file list
	 */
	@SuppressWarnings("deprecation")
	public void writeZipFile(File directoryToZip, List<File> fileList) {

		try {
			Date date = new Date();
			File fileZipFolder = new File(plugin.getDataFolder()
					.getAbsolutePath() + File.separator + "Reports");
			if (!fileZipFolder.exists()) {
				fileZipFolder.mkdirs();
			}

			FileOutputStream fos = new FileOutputStream(plugin.getDataFolder()
					.getAbsolutePath()
					+ File.separator
					+ "Reports"
					+ File.separator
					+ "Report"
					+ (date.getYear() + 1900)
					+ "."
					+ (date.getMonth() + 1)
					+ "."
					+ date.getDate()
					+ "."
					+ date.getHours()
					+ "."
					+ date.getMinutes()
					+ "."
					+ date.getSeconds() + ".zip");
			ZipOutputStream zos = new ZipOutputStream(fos);

			for (File file : fileList) {
				if (!file.isDirectory()) { // we only zip files, not directories
					addToZip(directoryToZip, file, zos);
				}
			}

			zos.close();
			fos.close();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
