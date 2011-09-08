package haskell.compiler;

import com.intellij.compiler.CompilerConfiguration;
import com.intellij.compiler.impl.CompilerUtil;
import com.intellij.openapi.compiler.CompileContext;
import com.intellij.openapi.compiler.CompileScope;
import com.intellij.openapi.compiler.TranslatingCompiler;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.fileTypes.FileTypeManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleType;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ModuleFileIndex;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.ui.Messages;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.Chunk;
import haskell.HaskellFileType;
import haskell.module.HaskellModuleType;
import haskell.util.Paths;
import org.jetbrains.annotations.NotNull;

import java.text.MessageFormat;
import java.util.*;

public final class HaskellCompiler implements TranslatingCompiler {

    private final Project project;

    public HaskellCompiler(Project project) {
        this.project = project;
    }

    public boolean isCompilableFile(VirtualFile file, CompileContext context) {
        FileType fileType = FileTypeManager.getInstance().getFileTypeByFile(file);
        return HaskellFileType.INSTANCE.equals(fileType);
    }

    private void verifyHaskellCode(Module module) {
    }

    public void compile(CompileContext context, Chunk<Module> moduleChunk, VirtualFile[] files, OutputSink sink) {
        Map<Module, List<VirtualFile>> mapModulesToVirtualFiles;
        if (moduleChunk.getNodes().size() == 1) {
            mapModulesToVirtualFiles = Collections.singletonMap(moduleChunk.getNodes().iterator().next(), Arrays.asList(files));
        } else {
            mapModulesToVirtualFiles = CompilerUtil.buildModuleToFilesMap(context, files);
        }
        for (Module module : moduleChunk.getNodes()) {
            verifyHaskellCode(module);    // has definitely to be somewhere else
            List<VirtualFile> moduleFiles = mapModulesToVirtualFiles.get(module);
            if (moduleFiles == null) {
                continue;
            }

            ModuleFileIndex index = ModuleRootManager.getInstance(module).getFileIndex();
            List<VirtualFile> toCompile = new ArrayList<VirtualFile>();
            List<VirtualFile> toCompileTests = new ArrayList<VirtualFile>();
            CompilerConfiguration configuration = CompilerConfiguration.getInstance(project);

            if (isAcceptableModuleType(module.getModuleType())) {
                for (VirtualFile file : moduleFiles) {
                    if (shouldCompile(file, configuration)) {
                        (index.isInTestSourceContent(file) ? toCompileTests : toCompile).add(file);
                    }
                }
            }

            if (!toCompile.isEmpty()) {
                compileFiles(context, module, toCompile, sink, false);
            }
            if (!toCompileTests.isEmpty()) {
                compileFiles(context, module, toCompileTests, sink, true);
            }

        }
    }

    protected static VirtualFile getMainOutput(CompileContext compileContext, Module module, boolean tests) {
        return tests
            ? compileContext.getModuleOutputDirectoryForTests(module)
            : compileContext.getModuleOutputDirectory(module);
    }

    private static void compileFiles(CompileContext context, Module module, List<VirtualFile> toCompile,
                                     OutputSink sink, boolean tests) {
        if (Paths.getLibVFile(module) == null)
            return;
        VirtualFile outputDir = getMainOutput(context, module, tests);
        List<OutputItem> output = new ArrayList<OutputItem>();
        for (VirtualFile file : toCompile) {
            for (GHCMessage message : LaunchGHC.getGHCMessages(outputDir, file.getPath(), module, tests)) {
                VirtualFile errFile = LocalFileSystem.getInstance().findFileByPath(message.getFileName());
                String url = errFile == null ? message.getFileName() : errFile.getUrl();
                context.addMessage(
                    message.getCategory(), message.getErrorMessage(),
                    url,
                    message.getRange().startLine, message.getRange().startColumn
                );
            }
        }
        sink.add(outputDir.getPath(), output, VfsUtil.toVirtualFileArray(toCompile));
    }

    private static boolean shouldCompile(VirtualFile file, CompilerConfiguration configuration) {
        return !configuration.isResourceFile(file);
    }

    private static boolean isAcceptableModuleType(ModuleType<?> moduleType) {
        return moduleType instanceof HaskellModuleType;
    }

    @NotNull
    public String getDescription() {
        return "Haskell compiler";
    }

    public boolean validateConfiguration(CompileScope compileScope) {
        VirtualFile[] files = compileScope.getFiles(HaskellFileType.INSTANCE, true);
        if (files.length == 0)
            return true;

        Set<Module> modules = new HashSet<Module>();
        for (VirtualFile file : files) {
            ProjectRootManager rootManager = ProjectRootManager.getInstance(project);
            Module module = rootManager.getFileIndex().getModuleForFile(file);
            if (module != null) {
                modules.add(module);
            }
        }

        Set<Module> noGhcModules = new HashSet<Module>();
        for (Module module : modules) {
            if (!isAcceptableModuleType(module.getModuleType()))
                continue;
            Sdk sdk = ModuleRootManager.getInstance(module).getSdk();
            if (sdk == null || !(sdk.getSdkType() instanceof HaskellSdkType)) {
                noGhcModules.add(module);
            }
        }

        if (!noGhcModules.isEmpty()) {
            if (noGhcModules.size() == 1) {
                Module module = noGhcModules.iterator().next();
                Messages.showErrorDialog(
                    project,
                    MessageFormat.format("Cannot compile Haskell files.\nPlease set up GHC for module ''{0}''.", module.getName()),
                    "Cannot Compile"
                );
            } else {
                StringBuilder buf = new StringBuilder();
                int i = 0;
                for (Module module : noGhcModules) {
                    if (i > 0)
                        buf.append(", ");
                    buf.append(module.getName());
                    i++;
                }
                Messages.showErrorDialog(
                    project,
                    MessageFormat.format("Cannot compile Haskell files.\nPlease set up GHC for modules ''{0}''.", buf.toString()),
                    "Cannot Compile"
                );
            }
            return false;
        }

        return true;
    }
}
