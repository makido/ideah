package haskell.formatter;

import com.intellij.formatting.Block;
import com.intellij.formatting.FormattingModel;
import com.intellij.formatting.FormattingModelBuilder;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.codeStyle.CodeStyleSettings;
import com.intellij.psi.formatter.DocumentBasedFormattingModel;
import com.intellij.psi.tree.IElementType;
import haskell.lexer.HaskellLexer;
import haskell.lexer.HaskellTokenTypes;
import haskell.util.LineColRange;
import haskell.util.ProcessLauncher;
import org.jetbrains.annotations.NotNull;

import java.io.BufferedReader;
import java.io.StringReader;
import java.util.*;

public final class HaskellFormattingModelBuilder implements FormattingModelBuilder {

    private static final Logger LOG = Logger.getInstance("haskell.formatter.HaskellFormattingModelBuilder");

    private static int prevSetBit(BitSet set, int from) {
        for (int i = from; i >= 0; i--) {
            if (set.get(i))
                return i;
        }
        return -1;
    }

    @NotNull
    public FormattingModel createModel(PsiElement element, CodeStyleSettings settings) {
        PsiFile file = element.getContainingFile();
        Project project = file.getProject();
        VirtualFile virtualFile = file.getVirtualFile();
        SortedMap<Integer, Block> map = new TreeMap<Integer, Block>();
        SortedSet<TextRange> functionRanges = new TreeSet<TextRange>(new Comparator<TextRange>() {
            public int compare(TextRange o1, TextRange o2) {
                return o1.getStartOffset() - o2.getStartOffset();
            }
        });
        String text = file.getText();
        if (virtualFile != null) {
            HaskellLexer lexer = new HaskellLexer();
            lexer.start(text);
            BitSet nonSpace = new BitSet();
            while (true) {
                IElementType type = lexer.getTokenType();
                if (type == null)
                    break;
                if (!HaskellTokenTypes.WHITESPACES.contains(type)) {
                    nonSpace.set(lexer.getTokenStart(), lexer.getTokenEnd());
                }
                lexer.advance();
            }
            try {
                // todo: возможно, мы парсим не последнюю версию файла - нужно форсировать сохранение?
                String path = virtualFile.getPath();
                String output = new ProcessLauncher(false, "D:\\home\\oleg\\haskell\\idea\\haskell\\haskell\\format.exe", path).getStdOut();
                BufferedReader rdr = new BufferedReader(new StringReader(output));
                while (true) {
                    String line = rdr.readLine();
                    if (line == null)
                        break;
                    LineColRange lcRange = new LineColRange(line);
                    TextRange range = lcRange.getRange(file);
                    if (range.isEmpty()) {
                        System.out.println("WTF?");
                    }
                    map.put(range.getStartOffset(), new HaskellBlock(range, Collections.<Block>emptyList()));
                    functionRanges.add(range);
                }
            } catch (Exception ex) {
                LOG.error(ex);
            }
            int i = 0;
            for (TextRange range : functionRanges) {
                int limit = range.getStartOffset();
                if (i < limit) {
                    int from = nonSpace.nextSetBit(i);
                    if (from >= 0 && from < limit) {
                        int to = prevSetBit(nonSpace, limit - 1);
                        if (to >= from) {
                            TextRange newRange = new TextRange(from, to + 1);
                            map.put(newRange.getStartOffset(), new HaskellBlock(newRange, Collections.<Block>emptyList()));
                        }
                    }
                }
                i = range.getEndOffset();
            }
            if (i < nonSpace.length()) {
                int from = nonSpace.nextSetBit(i);
                if (from >= 0) {
                    int to = prevSetBit(nonSpace, nonSpace.length() - 1);
                    if (to >= from) {
                        TextRange newRange = new TextRange(from, to + 1);
                        map.put(newRange.getStartOffset(), new HaskellBlock(newRange, Collections.<Block>emptyList()));
                    }
                }
            }
        }
        List<Block> blocks = new ArrayList<Block>(map.values());
        for (Block block : blocks) {
            System.out.println(block.getTextRange() + ": '" + block.getTextRange().substring(text) + "'");
        }
        Block rootBlock = new HaskellBlock(file.getTextRange(), blocks);
        return new DocumentBasedFormattingModel(rootBlock, project, settings, file.getFileType(), file);
    }

    public TextRange getRangeAffectingIndent(PsiFile file, int offset, ASTNode elementAtOffset) {
        return null;
    }
}
