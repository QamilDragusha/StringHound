package helper.androidMocks;

import helper.APKManager;

public class MockedApplicationInfo extends android.content.pm.ApplicationInfo {

    private String path;

    MockedApplicationInfo(APKManager apkManager) {
        path = apkManager.pathToResultsDirectory();
    }

    public String dataDir = path;

}
