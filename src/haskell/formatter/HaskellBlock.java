package haskell.formatter;

import com.intellij.formatting.*;
import com.intellij.openapi.util.TextRange;
import org.jetbrains.annotations.NotNull;

import java.util.List;

final class HaskellBlock implements Block {

    private final TextRange range;
    private final List<Block> subBlocks;

    HaskellBlock(TextRange range, List<Block> subBlocks) {
        this.range = range;
        this.subBlocks = subBlocks;
    }

    @NotNull
    public TextRange getTextRange() {
        return range;
    }

    @NotNull
    public List<Block> getSubBlocks() {
        return subBlocks;
    }

    public Wrap getWrap() {
        return null; // todo
    }

    public Indent getIndent() {
        return null; // todo
    }

    public Alignment getAlignment() {
        return null; // todo
    }

    public Spacing getSpacing(Block child1, Block child2) {
        return null; // todo
    }

    @NotNull
    public ChildAttributes getChildAttributes(int newChildIndex) {
        return new ChildAttributes(null, null); // todo
    }

    public boolean isIncomplete() {
        return false; // todo
    }

    public boolean isLeaf() {
        return false; // todo
    }
}
