package com.Ben12345rocks.VotingPlugin.Config;

import java.io.File;

import com.Ben12345rocks.AdvancedCore.Util.Annotation.AnnotationHandler;
import com.Ben12345rocks.AdvancedCore.Util.Annotation.ConfigDataString;
import com.Ben12345rocks.AdvancedCore.YML.YMLFile;
import com.Ben12345rocks.VotingPlugin.Main;

import lombok.Getter;

public class GUI extends YMLFile {
	/** The instance. */
	static GUI instance = new GUI();

	/** The plugin. */
	static Main plugin = Main.plugin;

	/**
	 * Gets the single instance of Config.
	 *
	 * @return single instance of Config
	 */
	public static GUI getInstance() {
		return instance;
	}

	public GUI() {
		super(new File(Main.plugin.getDataFolder(), "GUI.yml"));
	}

	public void loadValues() {
		new AnnotationHandler().load(getData(), this);
	}

	@Override
	public void onFileCreation() {
		plugin.saveResource("GUI.yml", true);
	}
	
	@ConfigDataString(path = "GUIMethod.Today")
	@Getter
	private String guiMethodToday = "CHEST";
	
	@ConfigDataString(path = "GUIMethod.TopVoter")
	@Getter
	private String guiMethodTopVoter = "CHEST";
	
	@ConfigDataString(path = "GUIMethod.Last")
	@Getter
	private String guiMethodLast = "CHEST";
	
	@ConfigDataString(path = "GUIMethod.Next")
	@Getter
	private String guiMethodNext = "CHEST";
	
	@ConfigDataString(path = "GUIMethod.Total")
	@Getter
	private String guiMethodTotal = "CHEST";
	
	@ConfigDataString(path = "GUIMethod.URL")
	@Getter
	private String guiMethodURL = "CHEST";
	
	@ConfigDataString(path = "GUIMethod.Best")
	@Getter
	private String guiMethodBest = "CHEST";
	
	@ConfigDataString(path = "GUIMethod.Streak")
	@Getter
	private String guiMethodStreak = "CHEST";
	
	@ConfigDataString(path = "GUIMethod.GUI")
	@Getter
	private String guiMethodGUI = "CHEST";
}
