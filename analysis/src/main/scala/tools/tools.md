# Tools

The following tools will be explained and documented below

## ClassLoaderUsageAnalysis

A simple tool for quickly checking how many class loaders are being 
instantiated and how many different class loaders are being used in a given app 
or given apps.

Analysis results will be written in a file as following (_only Apps with detected 
instantations will be listed_)
> *first_app_to_instantiate_classLoaders* : 7 instantiations of 5 different types
> *second_app_to_instantiate_classLoaders* : 9 instantiations of 4 different types

### Requirements

If apps in the APK format are to be  analyzed it is currently required to have
[pypy3](https://www.pypy.org) installed and made available in the path to be
accessible via the *pypy3* command. 

### Usage 

The arguments are as following (the order of arguments are not important):
```
(-apk/-jar) -p <path_to_the_directory/file> (-o <path_to_results_file>)
```
You can either analyze a single file or multiple files at once. If you want to analyze
a batch of files within a directory, you can simply specify the directory containing
the files. All top-level entires will be considered

If you only want either *.jar* or *.apk* file(s) to be considered for the analysis, you
can specify that by setting **either** the *-jar* **or** the *-apk* flag. By default, all
jar/apk files in a directory or a file directly specified will be considered. Using jar files
is considerably faster as APKs have to be repackaged first.

By default, the results will be documented in a *classLoaderAnalysis/results.txt* file
which will be created in the project folder. If you wish to specify a custom output file,
that can be done using the optional *-o* argument.

#### Examples

To analyze a single .jar file I specify:
```
-p /Users/me/Documents/File_to_analyze.jar
```

To analyze multiple jar-only files from a directory and log the result on my Desktop I specify:
```
-p /Users/me/Documents/My_Files_To_Analyze/ -jar -o /Users/me/Desktop/results.txt
```

