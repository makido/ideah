package haskell.compiler;

import com.intellij.openapi.projectRoots.*;
import com.intellij.openapi.util.Comparing;
import com.intellij.openapi.util.IconLoader;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.util.containers.HashMap;
import org.jdom.Element;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.io.*;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

// todo: config page - do not include classpath/sourcepath/etc
public final class HaskellSdkType extends SdkType {

    public static final HaskellSdkType INSTANCE = new HaskellSdkType();
    private static final Pattern VER_PATTERN = Pattern.compile("^(.*) version ([1234567890_.]*)(.*)$");
    public static final Icon GHC_ICON = IconLoader.getIcon("/haskell/haskell_16x16.png");

    public HaskellSdkType() {
        super("GHC");
    }

    public String suggestHomePath() {
        if (SystemInfo.isLinux) {
            return "/usr/lib/ghc/"; // todo: ???
        }
        if (SystemInfo.isWindows) {
            File[] roots = File.listRoots();
            if (roots.length > 0) {
                return new File(roots[0], "Haskell").getAbsolutePath();
            }
        }
        return null;
    }

    public static boolean checkForGhc(File path) {
        File bin = new File(path, "bin");
        if (!bin.exists())
            return false;
        File[] children = bin.listFiles(new FileFilter() {
            public boolean accept(File f) {
                if (f.isDirectory())
                    return false;
                return Comparing.strEqual(FileUtil.getNameWithoutExtension(f), "ghc");
            }
        });
        return children != null && children.length >= 1;
    }

    public boolean isValidSdkHome(String path) {
        return checkForGhc(new File(path));
    }

    public String suggestSdkName(String currentSdkName, String sdkHome) {
        String suggestedName;
        if (currentSdkName != null && currentSdkName.length() > 0) {
            suggestedName = currentSdkName;
        } else {
            String versionString = getVersionString(sdkHome);
            if (versionString != null) {
                suggestedName = "GHC " + getVersionNumber(versionString);
            } else {
                suggestedName = "Unknown";
            }
        }
        return suggestedName;
    }

    private final Map<String, String> cachedVersionStrings = new HashMap<String, String>();

    public final String getVersionString(String sdkHome) {
        if (cachedVersionStrings.containsKey(sdkHome)) {
            return cachedVersionStrings.get(sdkHome);
        }
        String versionString = getGhcVersion(sdkHome);
        if (versionString != null && versionString.length() == 0) {
            versionString = null;
        }

        if (versionString != null) {
            cachedVersionStrings.put(sdkHome, versionString);
        }

        return versionString;
    }

    @Nullable
    public static String getGhcVersion(String homePath) {
        if (homePath == null || !new File(homePath).exists()) {
            return null;
        }
        try {
            // todo: check for Linux
            String[] command = new String[] {homePath + File.separator + "bin" + File.separator + "ghc", "--version"};
            Process process = Runtime.getRuntime().exec(command);
            BufferedReader rdr = new BufferedReader(new InputStreamReader(process.getInputStream()));
            // todo: use threads to consume stdout/stderr
            String line = rdr.readLine();
            try {
                process.waitFor();
            } catch (InterruptedException e) {
                process.destroy();
            }
            return line;
        } catch (IOException ex) {
            // ignore
        }
        return null;
    }

    private static String getVersionNumber(String versionString) {
        Matcher matcher = VER_PATTERN.matcher(versionString);
        if (matcher.find()) {
            return matcher.group(2);
        }
        return versionString;
    }

    public AdditionalDataConfigurable createAdditionalDataConfigurable(SdkModel sdkModel, SdkModificator sdkModificator) {
        return null;
    }

    public void saveAdditionalData(SdkAdditionalData additionalData, Element additional) {
    }

    public String getPresentableName() {
        return "GHC";
    }

    @Override
    public Icon getIcon() {
        return GHC_ICON;
    }

    @Override
    public String adjustSelectedSdkHome(String homePath) {
        return super.adjustSelectedSdkHome(homePath); // todo: if 'bin' or 'ghc' selected, choose parent folder
    }

    @Override
    public void setupSdkPaths(Sdk sdk) {
        // todo: ???
    }
}
