package net.minecraft.network.chat;

/** Minimal test double for the mapped Minecraft chat component bridge. */
public record Component(String value) {
    public static Component literal(String value) {
        return new Component(value);
    }
}
