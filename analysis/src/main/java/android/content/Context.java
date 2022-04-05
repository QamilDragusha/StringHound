package android.content;

import android.content.res.AssetManager;

public class Context {

    public Context() {
        System.out.println("Initialized Context");
    }

    AssetManager getAssets() {
        return new AssetManager();
    }

}
