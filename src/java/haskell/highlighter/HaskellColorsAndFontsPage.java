package haskell.highlighter;

import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.options.colors.AttributesDescriptor;
import com.intellij.openapi.options.colors.ColorDescriptor;
import com.intellij.openapi.options.colors.ColorSettingsPage;
import haskell.HaskellFileType;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.Map;

public final class HaskellColorsAndFontsPage implements ColorSettingsPage {

    @NotNull
    public String getDisplayName() {
        return "Haskell";
    }

    @Nullable
    public Icon getIcon() {
        return HaskellFileType.HASKELL_ICON;
    }

    @NotNull
    public AttributesDescriptor[] getAttributeDescriptors() {
        return ATTRS;
    }

    private static final AttributesDescriptor[] ATTRS =
        new AttributesDescriptor[] {
            new AttributesDescriptor(HaskellSyntaxHighlighter.STRING_ID, HaskellSyntaxHighlighter.STRING_ATTR),
            new AttributesDescriptor(HaskellSyntaxHighlighter.NUMBER_ID, HaskellSyntaxHighlighter.NUMBER_ATTR),
            new AttributesDescriptor(HaskellSyntaxHighlighter.COMMENT_ID, HaskellSyntaxHighlighter.LINE_COMMENT_ATTR),
            new AttributesDescriptor(HaskellSyntaxHighlighter.ML_COMMENT_ID, HaskellSyntaxHighlighter.ML_COMMENT_ATTR),
            new AttributesDescriptor(HaskellSyntaxHighlighter.KEYWORD_ID, HaskellSyntaxHighlighter.KEYWORD_ATTR),
            new AttributesDescriptor(HaskellSyntaxHighlighter.KEY_SYM_ID, HaskellSyntaxHighlighter.KEYSYM_ATTR),
            new AttributesDescriptor(HaskellSyntaxHighlighter.TYCON_ID, HaskellSyntaxHighlighter.CON_ATTR),
            new AttributesDescriptor(HaskellSyntaxHighlighter.SYM_ID, HaskellSyntaxHighlighter.SYM_ATTR),
            new AttributesDescriptor(HaskellSyntaxHighlighter.ERROR_STRING_ID, HaskellSyntaxHighlighter.ERROR_STRING_ATTR),
            new AttributesDescriptor(HaskellSyntaxHighlighter.ERROR_NUMBER_ID, HaskellSyntaxHighlighter.ERROR_NUMBER_ATTR),
            new AttributesDescriptor(HaskellSyntaxHighlighter.ERROR_UNDEFINED_ID, HaskellSyntaxHighlighter.ERROR_UNDEFINED_ATTR)
        };

    @NotNull
    public ColorDescriptor[] getColorDescriptors() {
        return new ColorDescriptor[0];
    }

    @NotNull
    public SyntaxHighlighter getHighlighter() {
        return new HaskellSyntaxHighlighter();
    }

    @NonNls
    @NotNull
    public String getDemoText() {
        return
            "{-\n" +
            " Quicksort example\n" +
            "-}\n" +
            "module Main where\n\n" +
            "-- Not a real quicksort!\n" +
            "qsort :: Ord a => [a] -> [a]\n" +
            "qsort []     = []\n" +
            "qsort (p:xs) = qsort lesser ++ [p] ++ qsort greater\n" +
            "    where\n" +
            "        lesser  = filter (< p) xs\n" +
            "        greater = filter (>= p) xs\n\n" +
            "main :: IO ()\n" +
            "main = print $ \"Sorted list: \" ++ show (qsort [5, 3, 2, 4.5e-1])";
    }

    @Nullable
    public Map<String, TextAttributesKey> getAdditionalHighlightingTagToDescriptorMap() {
        return null;
    }
}
