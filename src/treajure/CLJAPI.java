package treajure;

import clojure.java.api.Clojure;
import clojure.lang.IFn;
import clojure.lang.Compiler;

import java.io.IOException;

public class CLJAPI {

    IFn require = Clojure.var("clojure.core", "require");

    public static Object loadFile (String filePath) throws IOException {
        return Compiler.loadFile(filePath);
    }

    public static IFn getFn(String ns, String varName){
        return Clojure.var(ns, varName);
    }

    public static Object applyFn(IFn f, Object... args){
        IFn apply = getFn("clojure.core", "apply");
        return apply.invoke(f, args);
    }
}

/*
USAGE:

1) First load your namespace:

CLJAPI.loadFile("/home/projects/.../my_ns.clj");


2) Then load any functions you're going to need:

IFn foo = CLJAPI.getFn("my-ns", "foo");
IFn bar = CLJAPI.getFn("my-ns", "bar");
...

3) Finally call the functions with appropriate arguments:

Object foo-res = CLJAPI.applyFn(foo, "arg1", "arg2");
Object bar-res = CLJAPI.applyFn(bar, "arg1", "arg2", "arg3");
...

 */