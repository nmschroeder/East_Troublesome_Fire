Please set up this directory following the instructions below to run the scripts in the East_Troublesome_Fire repository. When all the data sets are downloaded, there should be six directories within this one called 
* LANDFIRE
* TIGER_Colorado
* GOVTUNIT_Colorado_State_GPKG
* InterAgencyFirePerimeterHistory_All_Years_View
* Public_EventDataArchive_2020.gdb (directory, despite extension)
* osm/colorado


1. Create a directory within this downloads directory called **LANDFIRE**. Add to this directory the following CONUS datasets from https://landfire.gov/data/FullExtentDownloads
    * LC20_Elev_220.tif (Elevation)
    * LC19_F40_200.tif (Scott and Burgan 40 Fire Behavior Fuel Model data for 2019)
    * LC20_F40_200.tif (Scott and Burgan 40 Fire Behavior Fuel Model data for 2020)
    * LC22_F40_220.tif (Scott and Burgan 40 Fire Behavior Fuel Model data for 2022)

    Note that you can skip the 2019 and 2022 Scott and Burgan 40 Fire Behavior Fuel Model data sets if you do not want to create Figure S2. 

2. Please create a directory called **TIGER_Colorado** for the TIGER/Line shapefiles for Colorado roads. In this directory, please place 2021 TIGER/Line shapefiles for Larimer County, Grand County, and Boulder County. These data sets can be downloaded at the following websites:
    * https://catalog.data.gov/dataset/tiger-line-shapefile-2021-county-larimer-county-co-all-roads
    * https://catalog.data.gov/dataset/tiger-line-shapefile-2021-county-grand-county-co-all-roads
    * https://catalog.data.gov/dataset/tiger-line-shapefile-2021-county-boulder-county-co-all-roads
 
   These three downloads should generate three directories of data to be stored in **TIGER_Colorado** called
      * tl_2021_08013_roads (Boulder County)
      * tl_2021_08049_roads (Grand County)
      * tl_2021_08069_roads (Larimer County)

3. Next, please download boundary data for Colorado from the National Boundary Dataset at https://apps.nationalmap.gov/downloader/ or use the following URL to download the zip file:
https://prd-tnm.s3.amazonaws.com/StagedProducts/GovtUnit/GPKG/GOVTUNIT_Colorado_State_GPKG.zip
    * Please place the geopackage directory within the downloads directory to create **GOVTUNIT_Colorado_State_GPKG** with all the geopackage files within it
 
4. There are two directories to add from NIFC. One is the Interagency Fire Perimeter History dataset. Please download the directory **nterAgencyFirePerimeterHistory_All_Years_View** from https://data-nifc.opendata.arcgis.com/datasets/nifc::interagencyfireperimeterhistory-all-years-view/about/
    * Place this in the downloads directory so that all the shapefiles are stored within **InterAgencyFirePerimeterHistory_All_Years_View**
 
5. The next data set from NIFC is the Public Event Data Archive for 2020. This is a geodatabase that can be downloaded from https://data-nifc.opendata.arcgis.com/datasets/ea843f7f091f4c7f9743798b64c864be/about
    * These files should be stored within the directory **Public_EventDataArchive_2020.gdb** 
 
6. Finally, create another directory within the downloads directory called **osm/colorado**. We will use this to store Open Street Map data for the evacuation models.
    * The following files should be stored within **osm/colorado**
    * A .pbf file can be downloaded for Colorado streets from https://download.geofabrik.de/north-america/us/colorado.html
    * After running figure_00_prepare_LANDFIRE_data.R, please copy the file data/LC20_Elev_200_estes_park.tif to downloads/osm/colorado to include elevation in the evacuation models.
  
 
