package ideah.run;

import com.intellij.openapi.options.SettingsEditor;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;

final class ConfigurationEditor extends SettingsEditor<HaskellRunConfiguration> {

    private final JPanel mainPanel = new JPanel(new BorderLayout());
    private final ProgramParamsPanel programParams = new ProgramParamsPanel();

    ConfigurationEditor(Project project) {
        mainPanel.add(programParams);
        // todo: selection of main module
        // todo: runtime flags
    }

    protected void applyEditorTo(HaskellRunConfiguration s) {
        programParams.applyTo(s);
    }

    protected void resetEditorFrom(HaskellRunConfiguration s) {
        programParams.reset(s);
    }

    @NotNull
    protected JComponent createEditor() {
        return mainPanel;
    }

    protected void disposeEditor() {
    }
}
