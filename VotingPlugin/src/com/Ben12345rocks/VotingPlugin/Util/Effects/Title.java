package com.Ben12345rocks.VotingPlugin.Util.Effects;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

import org.bukkit.Bukkit;
import org.bukkit.ChatColor;
import org.bukkit.entity.Player;

/**
 * The Class Title.
 */
public class Title {
	
	/** The packet title. */
	/* Title packet */
	private static Class<?> packetTitle;
	
	/** The packet actions. */
	/* Title packet actions ENUM */
	private static Class<?> packetActions;
	
	/** The nms chat serializer. */
	/* Chat serializer */
	private static Class<?> nmsChatSerializer;
	
	/** The chat base component. */
	private static Class<?> chatBaseComponent;
	
	/** The title. */
	/* Title text and color */
	private String title = "";
	
	/** The title color. */
	private ChatColor titleColor = ChatColor.WHITE;
	
	/** The subtitle. */
	/* Subtitle text and color */
	private String subtitle = "";
	
	/** The subtitle color. */
	private ChatColor subtitleColor = ChatColor.WHITE;
	
	/** The fade in time. */
	/* Title timings */
	private int fadeInTime = -1;
	
	/** The stay time. */
	private int stayTime = -1;
	
	/** The fade out time. */
	private int fadeOutTime = -1;
	
	/** The ticks. */
	private boolean ticks = false;

	/** The Constant CORRESPONDING_TYPES. */
	private static final Map<Class<?>, Class<?>> CORRESPONDING_TYPES = new HashMap<Class<?>, Class<?>>();

	/**
	 * Instantiates a new title.
	 */
	public Title() {
		loadClasses();
	}

	/**
	 * Instantiates a new title.
	 *
	 * @param title
	 *            the title
	 */
	public Title(String title) {
		this.title = title;
		loadClasses();
	}

	/**
	 * Instantiates a new title.
	 *
	 * @param title
	 *            the title
	 * @param subtitle
	 *            the subtitle
	 */
	public Title(String title, String subtitle) {
		this.title = title;
		this.subtitle = subtitle;
		loadClasses();
	}

	/**
	 * Instantiates a new title.
	 *
	 * @param title
	 *            the title
	 */
	public Title(Title title) {
		// Copy title
		this.title = title.getTitle();
		this.subtitle = title.getSubtitle();
		this.titleColor = title.getTitleColor();
		this.subtitleColor = title.getSubtitleColor();
		this.fadeInTime = title.getFadeInTime();
		this.fadeOutTime = title.getFadeOutTime();
		this.stayTime = title.getStayTime();
		this.ticks = title.isTicks();
		loadClasses();
	}

	/**
	 * Instantiates a new title.
	 *
	 * @param title
	 *            the title
	 * @param subtitle
	 *            the subtitle
	 * @param fadeInTime
	 *            the fade in time
	 * @param stayTime
	 *            the stay time
	 * @param fadeOutTime
	 *            the fade out time
	 */
	public Title(String title, String subtitle, int fadeInTime, int stayTime,
			int fadeOutTime) {
		this.title = title;
		this.subtitle = subtitle;
		this.fadeInTime = fadeInTime;
		this.stayTime = stayTime;
		this.fadeOutTime = fadeOutTime;
		loadClasses();
	}

	/**
	 * Load classes.
	 */
	private void loadClasses() {
		if (packetTitle == null) {
			packetTitle = getNMSClass("PacketPlayOutTitle");
			packetActions = getNMSClass("PacketPlayOutTitle$EnumTitleAction");
			chatBaseComponent = getNMSClass("IChatBaseComponent");
			nmsChatSerializer = getNMSClass("ChatComponentText");
		}
	}

	/**
	 * Sets the title.
	 *
	 * @param title
	 *            the new title
	 */
	public void setTitle(String title) {
		this.title = title;
	}

	/**
	 * Gets the title.
	 *
	 * @return the title
	 */
	public String getTitle() {
		return this.title;
	}

	/**
	 * Sets the subtitle.
	 *
	 * @param subtitle
	 *            the new subtitle
	 */
	public void setSubtitle(String subtitle) {
		this.subtitle = subtitle;
	}

	/**
	 * Gets the subtitle.
	 *
	 * @return the subtitle
	 */
	public String getSubtitle() {
		return this.subtitle;
	}

	/**
	 * Sets the title color.
	 *
	 * @param color
	 *            the new title color
	 */
	public void setTitleColor(ChatColor color) {
		this.titleColor = color;
	}

	/**
	 * Sets the subtitle color.
	 *
	 * @param color
	 *            the new subtitle color
	 */
	public void setSubtitleColor(ChatColor color) {
		this.subtitleColor = color;
	}

	/**
	 * Sets the fade in time.
	 *
	 * @param time
	 *            the new fade in time
	 */
	public void setFadeInTime(int time) {
		this.fadeInTime = time;
	}

	/**
	 * Sets the fade out time.
	 *
	 * @param time
	 *            the new fade out time
	 */
	public void setFadeOutTime(int time) {
		this.fadeOutTime = time;
	}

	/**
	 * Sets the stay time.
	 *
	 * @param time
	 *            the new stay time
	 */
	public void setStayTime(int time) {
		this.stayTime = time;
	}

	/**
	 * Sets the timings to ticks.
	 */
	public void setTimingsToTicks() {
		ticks = true;
	}

	/**
	 * Sets the timings to seconds.
	 */
	public void setTimingsToSeconds() {
		ticks = false;
	}

	/**
	 * Send.
	 *
	 * @param player
	 *            the player
	 */
	public void send(Player player) {
		if (packetTitle != null) {
			// First reset previous settings
			resetTitle(player);
			try {
				// Send timings first
				Object handle = getHandle(player);
				Object connection = getField(handle.getClass(),
						"playerConnection").get(handle);
				Object[] actions = packetActions.getEnumConstants();
				Method sendPacket = getMethod(connection.getClass(),
						"sendPacket");
				Object packet = packetTitle.getConstructor(packetActions,
						chatBaseComponent, Integer.TYPE, Integer.TYPE,
						Integer.TYPE).newInstance(actions[2], null,
						fadeInTime * (ticks ? 1 : 20),
						stayTime * (ticks ? 1 : 20),
						fadeOutTime * (ticks ? 1 : 20));
				// Send if set
				if (fadeInTime != -1 && fadeOutTime != -1 && stayTime != -1)
					sendPacket.invoke(connection, packet);

				// Send title
				Object serialized = nmsChatSerializer.getConstructor(
						String.class).newInstance(
						ChatColor.translateAlternateColorCodes('&', title));
				packet = packetTitle.getConstructor(packetActions,
						chatBaseComponent).newInstance(actions[0], serialized);
				sendPacket.invoke(connection, packet);
				if (subtitle != "") {
					// Send subtitle if present
					serialized = nmsChatSerializer.getConstructor(String.class)
							.newInstance(
									ChatColor.translateAlternateColorCodes('&',
											subtitle));
					packet = packetTitle.getConstructor(packetActions,
							chatBaseComponent).newInstance(actions[1],
							serialized);
					sendPacket.invoke(connection, packet);
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * Update times.
	 *
	 * @param player
	 *            the player
	 */
	public void updateTimes(Player player) {
		if (Title.packetTitle != null) {
			try {
				Object handle = getHandle(player);
				Object connection = getField(handle.getClass(),
						"playerConnection").get(handle);
				Object[] actions = Title.packetActions.getEnumConstants();
				Method sendPacket = getMethod(connection.getClass(),
						"sendPacket");
				Object packet = Title.packetTitle.getConstructor(
						new Class[] { Title.packetActions, chatBaseComponent,
								Integer.TYPE, Integer.TYPE, Integer.TYPE })
						.newInstance(
								new Object[] {
										actions[2],
										null,
										Integer.valueOf(this.fadeInTime
												* (this.ticks ? 1 : 20)),
										Integer.valueOf(this.stayTime
												* (this.ticks ? 1 : 20)),
										Integer.valueOf(this.fadeOutTime
												* (this.ticks ? 1 : 20)) });
				if ((this.fadeInTime != -1) && (this.fadeOutTime != -1)
						&& (this.stayTime != -1)) {
					sendPacket.invoke(connection, packet);
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * Update title.
	 *
	 * @param player
	 *            the player
	 */
	public void updateTitle(Player player) {
		if (Title.packetTitle != null) {
			try {
				Object handle = getHandle(player);
				Object connection = getField(handle.getClass(),
						"playerConnection").get(handle);
				Object[] actions = Title.packetActions.getEnumConstants();
				Method sendPacket = getMethod(connection.getClass(),
						"sendPacket");
				Object serialized = nmsChatSerializer.getConstructor(
						String.class)
						.newInstance(
								ChatColor.translateAlternateColorCodes('&',
										this.title));
				Object packet = Title.packetTitle.getConstructor(
						new Class[] { Title.packetActions, chatBaseComponent })
						.newInstance(new Object[] { actions[0], serialized });
				sendPacket.invoke(connection, packet);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * Update subtitle.
	 *
	 * @param player
	 *            the player
	 */
	public void updateSubtitle(Player player) {
		if (Title.packetTitle != null) {
			try {
				Object handle = getHandle(player);
				Object connection = getField(handle.getClass(),
						"playerConnection").get(handle);
				Object[] actions = Title.packetActions.getEnumConstants();
				Method sendPacket = getMethod(connection.getClass(),
						"sendPacket");
				Object serialized = nmsChatSerializer.getConstructor(
						String.class).newInstance(
						ChatColor.translateAlternateColorCodes('&',
								this.subtitle));
				Object packet = Title.packetTitle.getConstructor(
						new Class[] { Title.packetActions, chatBaseComponent })
						.newInstance(new Object[] { actions[1], serialized });
				sendPacket.invoke(connection, packet);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * Broadcast.
	 */
	public void broadcast() {
		for (Player p : Bukkit.getOnlinePlayers()) {
			send(p);
		}
	}

	/**
	 * Clear title.
	 *
	 * @param player
	 *            the player
	 */
	public void clearTitle(Player player) {
		try {
			// Send timings first
			Object handle = getHandle(player);
			Object connection = getField(handle.getClass(), "playerConnection")
					.get(handle);
			Object[] actions = packetActions.getEnumConstants();
			Method sendPacket = getMethod(connection.getClass(), "sendPacket");
			Object packet = packetTitle.getConstructor(packetActions,
					chatBaseComponent).newInstance(actions[3], null);
			sendPacket.invoke(connection, packet);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Reset title.
	 *
	 * @param player
	 *            the player
	 */
	public void resetTitle(Player player) {
		try {
			// Send timings first
			Object handle = getHandle(player);
			Object connection = getField(handle.getClass(), "playerConnection")
					.get(handle);
			Object[] actions = packetActions.getEnumConstants();
			Method sendPacket = getMethod(connection.getClass(), "sendPacket");
			Object packet = packetTitle.getConstructor(packetActions,
					chatBaseComponent).newInstance(actions[4], null);
			sendPacket.invoke(connection, packet);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Gets the primitive type.
	 *
	 * @param clazz
	 *            the clazz
	 * @return the primitive type
	 */
	private Class<?> getPrimitiveType(Class<?> clazz) {
		return CORRESPONDING_TYPES.containsKey(clazz) ? CORRESPONDING_TYPES
				.get(clazz) : clazz;
	}

	/**
	 * To primitive type array.
	 *
	 * @param classes
	 *            the classes
	 * @return the class[]
	 */
	@SuppressWarnings("null")
	private Class<?>[] toPrimitiveTypeArray(Class<?>[] classes) {
		int a = classes != null ? classes.length : 0;
		Class<?>[] types = new Class<?>[a];
		for (int i = 0; i < a; i++)
			types[i] = getPrimitiveType(classes[i]);
		return types;
	}

	/**
	 * Equals type array.
	 *
	 * @param a
	 *            the a
	 * @param o
	 *            the o
	 * @return true, if successful
	 */
	private static boolean equalsTypeArray(Class<?>[] a, Class<?>[] o) {
		if (a.length != o.length)
			return false;
		for (int i = 0; i < a.length; i++)
			if (!a[i].equals(o[i]) && !a[i].isAssignableFrom(o[i]))
				return false;
		return true;
	}

	/**
	 * Gets the handle.
	 *
	 * @param obj
	 *            the obj
	 * @return the handle
	 */
	private Object getHandle(Object obj) {
		try {
			return getMethod("getHandle", obj.getClass()).invoke(obj);
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	/**
	 * Gets the method.
	 *
	 * @param name
	 *            the name
	 * @param clazz
	 *            the clazz
	 * @param paramTypes
	 *            the param types
	 * @return the method
	 */
	private Method getMethod(String name, Class<?> clazz,
			Class<?>... paramTypes) {
		Class<?>[] t = toPrimitiveTypeArray(paramTypes);
		for (Method m : clazz.getMethods()) {
			Class<?>[] types = toPrimitiveTypeArray(m.getParameterTypes());
			if (m.getName().equals(name) && equalsTypeArray(types, t))
				return m;
		}
		return null;
	}

	/**
	 * Gets the version.
	 *
	 * @return the version
	 */
	private String getVersion() {
		String name = Bukkit.getServer().getClass().getPackage().getName();
		String version = name.substring(name.lastIndexOf('.') + 1) + ".";
		return version;
	}

	/**
	 * Gets the NMS class.
	 *
	 * @param className
	 *            the class name
	 * @return the NMS class
	 */
	private Class<?> getNMSClass(String className) {
		String fullName = "net.minecraft.server." + getVersion() + className;
		Class<?> clazz = null;
		try {
			clazz = Class.forName(fullName);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return clazz;
	}

	/**
	 * Gets the field.
	 *
	 * @param clazz
	 *            the clazz
	 * @param name
	 *            the name
	 * @return the field
	 */
	private Field getField(Class<?> clazz, String name) {
		try {
			Field field = clazz.getDeclaredField(name);
			field.setAccessible(true);
			return field;
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	/**
	 * Gets the method.
	 *
	 * @param clazz
	 *            the clazz
	 * @param name
	 *            the name
	 * @param args
	 *            the args
	 * @return the method
	 */
	private Method getMethod(Class<?> clazz, String name, Class<?>... args) {
		for (Method m : clazz.getMethods())
			if (m.getName().equals(name)
					&& (args.length == 0 || ClassListEqual(args,
							m.getParameterTypes()))) {
				m.setAccessible(true);
				return m;
			}
		return null;
	}

	/**
	 * Class list equal.
	 *
	 * @param l1
	 *            the l 1
	 * @param l2
	 *            the l 2
	 * @return true, if successful
	 */
	private boolean ClassListEqual(Class<?>[] l1, Class<?>[] l2) {
		boolean equal = true;
		if (l1.length != l2.length)
			return false;
		for (int i = 0; i < l1.length; i++)
			if (l1[i] != l2[i]) {
				equal = false;
				break;
			}
		return equal;
	}

	/**
	 * Gets the title color.
	 *
	 * @return the title color
	 */
	public ChatColor getTitleColor() {
		return titleColor;
	}

	/**
	 * Gets the subtitle color.
	 *
	 * @return the subtitle color
	 */
	public ChatColor getSubtitleColor() {
		return subtitleColor;
	}

	/**
	 * Gets the fade in time.
	 *
	 * @return the fade in time
	 */
	public int getFadeInTime() {
		return fadeInTime;
	}

	/**
	 * Gets the fade out time.
	 *
	 * @return the fade out time
	 */
	public int getFadeOutTime() {
		return fadeOutTime;
	}

	/**
	 * Gets the stay time.
	 *
	 * @return the stay time
	 */
	public int getStayTime() {
		return stayTime;
	}

	/**
	 * Checks if is ticks.
	 *
	 * @return true, if is ticks
	 */
	public boolean isTicks() {
		return ticks;
	}
}