# Tools

The following tools will be explained and documented below:
 - ClassLoaderUsageAnalysis
 - ClassDeobfuscationBenchmark

## ClassLoaderUsageAnalysis

A simple tool for quickly analyzing how many and which class loaders are being 
instantiated in a given app or given apps.

Analysis results will be returned in form of a *.csv* table that lists the results as 
following:

| Sum                      | 8   |
|--------------------------|-----| 
| com.example.ClassLoader1 | 6   |
| com.example.ClassLoader2 | 1 |
| ... | ...|

> The first row holds the total amound of class loader instances found while any other
> row holds a class loader type found as well as it's found instances

Additionally, a logfile will be created which holds information on how many of the
given files were successfully analyzed.

### Requirements

If apps in the APK format are to be  analyzed it is currently required to have
[pypy3](https://www.pypy.org) installed and made available in the path to be
accessible via the *pypy3* command. 

### Usage 

The arguments are as following (the order of arguments are not important):
```
-p <path_to_the_directory/file> (-apk/-jar) (-o <path_to_results_file>) (-v)
```

###### Path
You can either analyze a single file or multiple files at once. If you want to analyze
a batch of files within a directory, you can simply specify the directory containing
the files. All top-level entires will be considered. You can also specify the path of a
*.txt* file containing all paths to files you want to analyze. The paths contained in 
this file must be seperated by line breaks.

###### APK/JAR/AAR Filter
If you only want either *.jar*, *.aar* or *.apk* file(s) to be considered for the analysis, you
can specify that by setting **either** the *-jar* **or** the *-apk* **or** the *-lib* flag. By default, all
jar/apk files in a directory or a file directly specified will be considered. Using jar files
is considerably faster as APKs have to be repackaged first.

###### Output Directory
By default, the results will be documented in a *classLoaderAnalysis* directory
which will be created in the project folder. If you wish to specify a custom output directory,
that can be done using the optional *-o* argument. **Each app / file** analyzed will result in an *.csv* 
formatted conclusion named analogous to the app / file.

*Naming Example:*
> An app to be analzed is named *fancy_app_analyze_me.jar*. This will yield a result-file named
> *fancy_app_analyze_me.jar.csv* in the ouput specified (if any) 

###### Logging
As console logging is expensive (especially on parallelized systems) most logging outputs will 
be omitted if not specifically set otherwise via the *-v* / *-verbose* flag.

#### Examples

To analyze a single .jar file I specify:
```
-p /Users/me/Documents/File_to_analyze.jar
```

To analyze multiple jar-only files from a directory and log the result on my Desktop I specify:
```
-p /Users/me/Documents/My_Files_To_Analyze/ -jar -o /Users/me/Desktop/results.txt
```

To analyze multiple apk-only files from a bunch of files listed in a .txt file I specify:
```
-p /Users/me/Documents/paths.txt -apk
```

## ClassDeobfuscationBenchmark

A benchmark tool which analyzes given APKs with the underlying *ClassHound* implementation
and exports a detailed overview of the type-specific recall.

### Requirements

In order to run _ClassHound_, both _apkTool_ as well as _pypy3_ have to be installed and 
made available in the path.

### Usage

The parameters required to execute the benchmark are as follows and can be ordered as desirerd.

```
-basePath <path_to_the_base_directory> -pathsFilePath <path_to_the_paths_csv> -logPath <path_to_the_log_csv>
```


###### BasePath

The directory to which the paths in the given *pathsFilePath* are relative to.

###### PathsFilePath

The path to the *.csv* file that contains all apk file names relative to the *basePath*.

###### LogPath

The path to the *logFile* to be created.

##### Example

```
-basePath /Users/me/Documents/APKs/ -logPath /Users/me/Desktop/class_benchmark_results.csv -pathsFilePath /Users/me/Desktop/paths.csv 
```
