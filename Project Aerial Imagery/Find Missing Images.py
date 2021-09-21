__Author__='Terrance Randolph'

import arcpy,os
from shutil import copy
arcpy.env.overwriteOutput = False

def pathList():
        cnt = 0
        pathList  = []
        while cnt < 1:          
            i=raw_input('Folder Path:  ').replace("\\","/")
            pathList.append(i)
            x=raw_input('More Folders? yes/no: ')
            if x == 'yes':
                continue
            elif x == 'no':
                cnt+=1
            else:
                print 'Invalid input.. Either yes or no.'
                cnt+=1
        return pathList

def tifList(listOfPaths):
        tiffList = []
        for i in listOfPaths:
            arcpy.env.workspace = i
            k = [str(tiff.replace("u","")) for tiff in arcpy.ListRasters("*","TIF")]
            tiffList.extend(k)
        return set(tiffList)

def copyImages(ListOfPaths,ListOfMissingImages,Destination):
    # Looop through unrectified and missing tiff
    for i in ListOfPaths:
        for x in ListOfMissingImages:
            if os.path.exists(str(i)+'/'+'u'+str(x)) == True:
                # If path exist, then copy to new dir
                copy(str(i)+'/'+'u'+str(x),Destination)
                print 'Copied: {}'.format(x)
            else:
                print 'No match for {}... check Unrectified folder manually'.format(x)

print "Enter Folder Path. Format Ex. P:\\charleston_45019\\45019_TIFF_1973"
print '\nUnrectified Folder First.'

UnPath = pathList()

print "Enter Folder Destination. Format Ex. P:\\charleston_45019\\45019_TIFF_1973\\Name of folder"
Destination = raw_input('Destination Folder?: ').replace("\\","/")
if os.path.exists(Destination) != True:
        os.mkdir(Destination)
else:
        print '{} Already created'.format(str(Destination))

print "\nEnter Folder Path. Format Ex. P:\\charleston_45019\\45019_TIFF_1973"
print '\nRectified Folder Second.'

RecPath = pathList()

UNR = tifList(UnPath)
print '\nUnrectified list completed'
REL = tifList(RecPath)
print 'Rectified list completed\n'

UNRl = len(UNR)
RELl= len(REL)
mtNum = UNRl - RELl

print "Total number of Unrectified TIF: "+str(UNRl)+"\n"+"Total number of Rectified TIF: "+str(RELl)
print "\nTotal number of Missing TIF: "+str(mtNum)

if UNRl is not RELl:
    MissingTiffs = UNR - REL
    copyImages(UnPath,MissingTiffs,Destination)
    with open(Destination+'/Missing_Images.csv','w')as f:
            # image number + interation
            for i,x in zip(MissingTiffs,range(1,len(MissingTiffs)+1)):
                    print "\nMissing TIF's are: " + str(i),str(x)
                    f.write(str(i)+'\n')
else:
    print "No missing TIF's"

