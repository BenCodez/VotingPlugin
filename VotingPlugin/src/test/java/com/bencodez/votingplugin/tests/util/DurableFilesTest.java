package com.bencodez.votingplugin.tests.util;

import com.bencodez.votingplugin.util.DurableFiles;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

class DurableFilesTest {
	@Test
	void identifiesWindowsWithoutMistakingDarwin() {
		assertTrue(DurableFiles.isWindowsName("Windows 11"));
		assertTrue(DurableFiles.isWindowsName("Windows Server 2022"));
		assertFalse(DurableFiles.isWindowsName("Darwin"));
		assertFalse(DurableFiles.isWindowsName("Linux"));
	}
}
