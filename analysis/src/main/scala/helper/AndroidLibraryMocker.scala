package helper

import android.content.Context
import android.content.pm.{ApplicationInfo, PackageInfo, PackageManager}
import org.mockito.Mockito.{mock, when, withSettings}
import android.content.res.{AssetManager, Resources}
import android.os.Bundle
import org.mockito.ArgumentMatchers.{anyInt, anyString}
import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.Answer

class AndroidLibraryMocker(apkManager: APKManager) {

  def mockContext(classLoader: ClassLoader) : Context = {
    val instance = mock(classOf[Context])

    when(instance.getPackageManager).thenAnswer(new Answer[Any](){
      def answer(invocation: InvocationOnMock): Object = {
        packageManager
      }
    })

    // TODO qamil: Might be worth it to think of a way to include the actual package name somehow
    when(instance.getPackageName).thenAnswer(new Answer[Any](){
      def answer(invocation: InvocationOnMock): Object = {
        ""
      }
    })

    when(instance.getAssets).thenAnswer(new Answer[Any](){
      def answer(invocation: InvocationOnMock) : Object = {
        assetManager
      }
    })

    when(instance.getFilesDir).thenAnswer(new Answer[Any]() {
      def answer(invocationOnMock: InvocationOnMock) : Object = {
        apkManager.dataDirectory
      }
    })

    when(instance.getCacheDir).thenAnswer(new Answer[Any]() {
      def answer(invocationOnMock: InvocationOnMock) : Object = {
        apkManager.cacheDirectory
      }
    })

    when(instance.getDir(anyString(), anyInt())).thenAnswer(new Answer[Any]() {
      def answer(invocation: InvocationOnMock) : Object = {
        val name : String = invocation.getArgument(0)
        val mode : Int = invocation.getArgument(1)
        println(s"name $name with mode $mode")
        // qamil TODO: create a new directory
        apkManager.cacheDirectory
      }
    })

    when(instance.getClassLoader).thenAnswer(new Answer[Any]() {
      def answer(invocationOnMock: InvocationOnMock) : Object = {
        // qamil TODO: Should we do that?
        classLoader
      }
    })

    when(instance.getApplicationInfo).thenAnswer(new Answer[Any]() {
      def answer(invocation: InvocationOnMock): Object = {
        applicationInfo
      }
    })

    when(instance.getResources).thenAnswer(new Answer[Any]() {
      def answer(invocation: InvocationOnMock): Object = {
        resources
      }
    })

    instance
  }

  def assetManager : AssetManager = {
    val assetManagerInstance = mock(classOf[AssetManager])

    when(assetManagerInstance.open(anyString())).thenAnswer(new Answer[Any](){
      def answer(invocation: InvocationOnMock): Object = {
        val path : String = invocation.getArgument(0)
        // assuming the file is already located in the resources folder
        apkManager.getAssetStream(path)
      }
    })

    assetManagerInstance
  }

  def applicationInfo : ApplicationInfo = {
    val applicationInfoInstance = mock(classOf[ApplicationInfo])

    val appInfoDataDirField = applicationInfoInstance.getClass.getDeclaredField("dataDir")
    appInfoDataDirField.setAccessible(true)
    appInfoDataDirField.set(applicationInfoInstance, apkManager.pathToDataDir)

    // metaData is only defined in PackageItemInfo, the superclass of ApplicationInfo
    val appInfoMetaDataField = applicationInfoInstance.getClass.getSuperclass.getDeclaredField("metaData")
    appInfoMetaDataField.setAccessible(true)
    appInfoMetaDataField.set(applicationInfoInstance, bundle)

    applicationInfoInstance
  }

  def packageInfo : PackageInfo = {
    val packageInfoInstance = mock(classOf[PackageInfo])

    val packageInfoVersionCodeField = packageInfoInstance.getClass.getDeclaredField("versionCode")
    packageInfoVersionCodeField.setAccessible(true)
    packageInfoVersionCodeField.set(packageInfoInstance, 0)

    packageInfoInstance
  }

  def packageManager : PackageManager = {
    val packageManagerInstance = mock(classOf[PackageManager])

    when(packageManagerInstance.getPackageInfo(anyString(), anyInt())).thenAnswer(new Answer[Any](){
      def answer(invocation: InvocationOnMock): Object = {
        packageInfo
      }
    })

    when(packageManagerInstance.getApplicationInfo(anyString(), anyInt())).thenAnswer(new Answer[Any]() {
      def answer(invocation: InvocationOnMock): Object = {
        applicationInfo
      }
    })

    packageManagerInstance
  }

  def bundle : Bundle = {
    val bundleInstance = mock(classOf[Bundle])

    when(bundleInstance.getString(anyString())).thenAnswer(new Answer[Any](){
      def answer(invocation: InvocationOnMock): Object = {
        ""
      }
    })

    when(bundleInstance.getInt(anyString())).thenAnswer(new Answer[Any](){
      def answer(invocation: InvocationOnMock): Int = {
        1
      }
    })

    bundleInstance
  }

  def resources : Resources = {
    val resourcesInstance = mock(classOf[Resources])
    resourcesInstance
  }

}
