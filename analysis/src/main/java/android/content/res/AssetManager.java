package android.content.res;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

public class AssetManager {

    InputStream open(String path) {
        return new ByteArrayInputStream(new byte[]{1,2,3,4});
    }

}
