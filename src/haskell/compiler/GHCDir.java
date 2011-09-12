package haskell.compiler;

class GHCDir {

    final String name;
    final String[] version;

    GHCDir(String name) {
        this.name = name;
        version = name.split("[^0-9]");
    }
}
