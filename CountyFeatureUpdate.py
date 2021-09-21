_Author_ = 'GIS Analyst : Terrance Randolph'

import sys, zipfile, arcpy, os, traceback
from os.path import isdir, join, normpath, split


#######################################################                   
# how to get one feature to read in list.NOT WORKING!!!
def listLen(infileList):
    if len(infileList)> 1:
        flist = []
        for i in infileList:
            paths = i.replace("'","")
            flist.append(paths)
        return(flist)
    else:
        return(infileList)
#######################################################

    
# Function:  unzipping the zip file
def unzip(path, zip):
    # If the output location does not yet exist, create it
    if not isdir(path):
        os.makedirs(path)
        arcpy.AddMessage(path + " Created!")
    for each in zip.namelist():
        if os.path.exists(join(path, each)) != True:
            arcpy.AddMessage("Extracting " + each)
                
            # Check to see if the item was written to the zip file with an
            # archive name that includes a parent directory. If it does, create
            # the parent folder in the output workspace and then write the file,
            # otherwise, just write the file to the workspace.
            
            if not each.endswith('/'): 
                root, name = split(each)
                directory = normpath(join(path, root))
                if not isdir(directory):
                    os.makedirs(directory)
                file(join(directory, name), 'wb').write(zip.read(each))
        else:
            arcpy.AddMessage(join(path, each) + "   Already Exsists!")


# Function: reprojecting shpfiles in unzipped/extracted folder
def defineProjection(extfol):
        arcpy.env.workspace = extfol
        arcpy.env.overwriteOutput = True
        datasets = arcpy.ListDatasets(feature_type='feature')
        datasets = [''] + datasets if datasets is not None else []
        for ds in datasets:
            for shp in arcpy.ListFeatureClasses(feature_dataset=ds):
                infeature = os.path.join(extfol, ds, shp)
                ProjectFeat = infeature.replace('.shp','')+"_reprojected.shp"
                ChkProject = ProjectFeat.replace('reprojected_','')
                # get the coordinate system by describing a feature class
                dsc = arcpy.Describe(shp)
                #psc = arcpy.Describe(ProjectFeat)
                name_coord_sys = dsc.spatialReference.name
                # var's for Defined projections and projected spatial ref..
                # --- state_coord_sys = arcpy.SpatialReference('NAD 1983 StatePlane South Carolina FIPS 3900 (Intl Feet)')
                # --- coord_sys_17N = arcpy.SpatialReference('NAD 1983 UTM Zone 17N')
                # --- arcpy.AddMessage(arcpy.Describe(ChkProject).spatialReference.name)
                state_coord_sys = arcpy.SpatialReference(2273)
                coord_sys_17N = arcpy.SpatialReference(26917)

                # CONDITIONAL check if spatial referance of Shp is unknown & define: StatePlane & project: 17N
                if dsc.spatialReference.Name == "Unknown":
                    # Define Spatial Reference State
                    arcpy.AddMessage("\n"+"Defining  " + shp)
                    # Define Projecttion Spatial Reference StatePlane SC
                    arcpy.DefineProjection_management(infeature, state_coord_sys)
                    arcpy.AddMessage("\n" + "  DEFINED PROJECTON IS NOW : " +
                                     arcpy.Describe(shp).spatialReference.name + "\n")
                    arcpy.AddMessage("  PROJECTING TO NAD 83 Zone 17N" + "\n")
                    # Project Spatial Reference 17N
                    arcpy.Project_management(infeature, ProjectFeat, coord_sys_17N)
                    arcpy.AddMessage("\n" + "  PROJECTON TRANSFORMED IS NOW : " +
                                     arcpy.Describe(ChkProject).spatialReference.name + "\n")
                    # remove old shp from directory
                    arcpy.Delete_management(infeature,"")
                    arcpy.AddMessage("\n" + shp +" Removed")
                    
                    # CONDITIONAL check if spatial referance  of Shp is State plane & Project 17N
                elif name_coord_sys == state_coord_sys.name or name_coord_sys != "Unknown" or name_coord_sys != coord_sys_17N.name :
                    arcpy.AddMessage("\n" + shp + "  PROJECTON IS : " +
                                     arcpy.Describe(shp).spatialReference.name + "\n")
                    arcpy.AddMessage("  PROJECTING "+shp+" TO NAD 83 Zone 17N" + "\n")
                    # Project Spatial Reference 17N
                    arcpy.Project_management(infeature, ProjectFeat, coord_sys_17N)
                    arcpy.AddMessage("\n" + shp + "  PROJECTON TRANSFORMED TO: " +
                                     arcpy.Describe(ChkProject).spatialReference.name + "\n")
                    # remove old shp from directory
                    arcpy.Delete_management(infeature,"") 
                    arcpy.AddMessage("\n" + shp +" Removed")
                        
                    # CONDITIONAL is NAD 83 17N
                elif name_coord_sys == coord_sys_17N.name:
                    # Shp is already projected to 17N
                    arcpy.AddMessage("\n" +shp + "  PROJECTON IS : " +
                                     arcpy.Describe(shp).spatialReference.name + "\n")
                    # rename to reprojected
                    os.rename(infeature, ProjectFeat)
                    # current name
                    arcpy.AddMessage("\n" + shp + " RENAMED TO: " + ProjectFeat)
                            
                    # ??? What Happend.. Investagate!
                else:
                    # Something weired happened
                    arcpy.AddMessage("\n" + shp + "  projection is already  " +
                                     arcpy.Describe(shp).spatialReference.name + "\n")
                            
# Function: copying projected files from unzipped/extracted folder to SDE
def copyShp(extfol,Gdb):
        arcpy.env.workspace = extfol
        datasets = arcpy.ListDatasets(feature_type='feature')
        datasets = [''] + datasets if datasets is not None else []
        for ds in datasets:
            for shp in arcpy.ListFeatureClasses(feature_dataset=ds):
                shp = Chk_Name_Int(shp)
                # Determine the new output feature class path and name
                out_featureclass1 = os.path.join(Gdb, os.path.splitext(shp)[0])
                out_featureclass2 = os.path.split(out_featureclass1)[0] + shp 
                out_featclass = out_featureclass2 + "_Repojected"
                # Determin dirctory for shp for deletion
                infeature = os.path.join(extfol, ds, shp)
                # Execute CopyFeatures for each input shapefile
                if arcpy.Exists(out_featclass) != True:
                    arcpy.AddMessage("Copying "+ shp + " to "+
                                     "\n"+out_featclass)
                    arcpy.CopyFeatures_management(shp, out_featclass)
                    arcpy.AddMessage(" Copied! "+"\n")
                    # remove old shp from directory
                    #arcpy.Delete_management(infeature,"") 
                    #arcpy.AddMessage("\n" + shp +" Removed")
                else:
                    arcpy.AddMessage(shp + "  already exists" +
                                     "\n"+ " in : " +out_featclass)
  
# function to correct exceptions of SDE numbers in begining of name
def Chk_Name_Int(fn): #used in copyShp
    # create number search list
    num = '0,1,2,3,4,5,6,7,8,9'
    # get first index of shp
    if set(fn[0]).intersection(num) == True:
        #print('has integer')
        # rename to had integer
        fnN = fn.replace(fn[0],'INT_')
        os.rename(fn, fn.replace(fn,fnN))
        return fnN
    else:
        #print('does not have integer')
        return fn

#Function: unzip, define projection and copy                             
def Unzip_DefineProj_Copy(infileList,extfol,Gdb):
	try:
            # run len() checker on list because it will through errors
            # Flt_infileList = listLen(infileList)
            # Create the zipfile handle for reading and unzip it
            for infile in infileList.split(";"):
                zip = zipfile.ZipFile(infile, 'r')
                unzip(extfol, zip)
                zip.close()
                # after everything is unziped
            defineProjection(extfol)
            copyShp(extfol,Gdb)

	except:
            # Return any Python specific errors and any error returned by the geoprocessor
            tb = sys.exc_info()[2]
            tbinfo = traceback.format_tb(tb)[0]
            pymsg = "PYTHON ERRORS:\nTraceback Info:\n" + tbinfo + "\nError Info:\n    " + \
                    str(sys.exc_type)+ ": " + str(sys.exc_value) + "\n"
            arcpy.AddError(pymsg)
            msgs = "GP ERRORS:\n" + arcpy.GetMessages(2) + "\n"
            arcpy.AddError(msgs)

# Check to see if filed is Null, if empty skip.
def checkNull(param, func):
        if param:
            arcpy.AddMessage("Not blank")
            func
        else:
            arcpy.AddMessage(param + " Null")
            arcpy.AddMessage("skipping" + param)
            
if __name__ == '__main__':

    ### --- Set variables & run functions --- ###
    ##############################################
    infileListA  = arcpy.GetParameterAsText(0)
    extfolA      = arcpy.GetParameterAsText(1)
    GdbA         = arcpy.GetParameterAsText(2)
    infileListM  = arcpy.GetParameterAsText(3)
    extfolM      = arcpy.GetParameterAsText(4)
    GdbM         = arcpy.GetParameterAsText(5)
    infileListP  = arcpy.GetParameterAsText(6)
    extfolP      = arcpy.GetParameterAsText(7)
    GdbP         = arcpy.GetParameterAsText(8)
    infileListZ  = arcpy.GetParameterAsText(9)
    extfolZ      = arcpy.GetParameterAsText(10)
    GdbZ         = arcpy.GetParameterAsText(11)
    
    ##############################################
            ## --- Address points --- ##
    checkNull(infileListA,
              Unzip_DefineProj_Copy(infileListA.replace("'",""),extfolA,GdbA))
            ## --- Municipalities --- ##
    checkNull(infileListM,
              Unzip_DefineProj_Copy(infileListM.replace("'",""),extfolM,GdbM))
            ## --- Parcels --- ##
    checkNull(infileListP,
              Unzip_DefineProj_Copy(infileListP.replace("'",""),extfolP,GdbP))
            ## --- Zoning --- ##
    checkNull(infileListZ,
              Unzip_DefineProj_Copy(infileListZ.replace("'",""),extfolZ,GdbZ))
    ##############################################


    










