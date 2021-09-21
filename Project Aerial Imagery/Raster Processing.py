__Author__ = 'Terrance Randolph'


def sendmail(content):
    import smtplib
    if isinstance(content,str):
        mail = smtplib.SMTP('smtp.gmail.com',587)# or port 465
        mail.ehlo()# extended smtp server or hello for standered server
        mail.starttls()

        # mail.login('email','password')
        mail.login('Email@gmail.com','password')

        # mail.sendmail('fromemail','reciver',content)
        mail.sendmail('Email@gmail.com','Email@gmail.com',content)
        mail.close()
        print 'Email sent'
    else:
        print 'Content is not a string'

def log(string):
    from time import gmtime, strftime
        # Check if parameter is a string
    if isinstance(string,str):
        with open(Outraster+'Logs.txt','w') as logs:
            logs.write(string + '\n' + str(strftime("%a, %d %b %Y %I:%M:%S ", gmtime()))+'\n')
    else:
        print 'Input is not a string!'

def convertRaster(Inraster,Outraster):
    import arcpy,os
    arcpy.env.workspace = Inraster
    arcpy.env.overwriteOutput = False
    try:
        for tiff in arcpy.ListRasters("*", "TIF"):
                # For every image in the folder
                # Check if image is in converted folder
                # Check to see if band is 3 or 1, if so convert approperately 
            if os.path.isfile(Outraster + tiff) == True:
                print '{} Already converted to 8bit! : {}'.format(tiff,Outraster)
            elif os.path.isfile(Outraster + tiff) != True and str(arcpy.Describe(tiff).bandCount) == '3':
                arcpy.CopyRaster_management(tiff,Outraster+tiff,"", "",
                "", "", "", "8_BIT_UNSIGNED", "", "RGBToColormap")
                print 'converted '+tiff+"\n"
            elif os.path.isfile(Outraster + tiff) != True and str(arcpy.Describe(tiff).bandCount) == '1':
                arcpy.MosaicToNewRaster_management(tiff,Outraster,tiff,
                "","8_BIT_UNSIGNED", "", "1","LAST","FIRST")
                print 'converted '+tiff+"\n"
            else:
                print 'Check Tiff:  {}  Band Number'.format(tiff)
    except arcpy.ExecuteError:
        Cvt = log(str('Converter function Error\n\n'+tiff + " : "\
        +str(arcpy.ExecuteError)+"\n\n"+tiff + " : "+arcpy.GetMessages()+"\n\n"))
        print 'Error... Check Logs {}'.format(Outraster+"/Logs.txt")  

def cmd(Tilefolder, folderName):
    import subprocess as sub
    import os
    try:
            # Change dir, because sub.call() is commad window commands
            # and the dir change is the same as (cd "path")
    	os.chdir(Tilefolder)
    	sub.call('maptiler -o '+folderName+' -f png8 -zoom 8 18 *.tif',shell=True)
        sendmail(folderName+'...Images are Tiled!... ')
    	return 'Tiles Completed'
    except:
        print 'Images could not be Tiled... check cmd function'
        sendmail(folderName+'Images could not be Tiled... check cmd function')

def modifyHTML(tileDir,old,new):
    import os
    try:
        for i in os.listdir(tileDir):
                # Find the google html doc. 
            if i=='googlemaps.html':
                with open(tileDir+'googlemaps.html','r') as f:
                    lines = f.read()
                # Write it to a Txt doc. while replacing old with new
        with open(tileDir + 'sample.txt','w') as j:
            j.write(lines.replace(old,new))
        with open(tileDir + 'sample.txt','r') as r:   
            read = r.read()
                # Re-write the modified Txt doc. to a html doc.
        with open(tileDir+"googlemapsModified.html",'w') as final:
            final.write(read)
            # Storing the path of the original google html & sample Txt
        oldG,smp = tileDir + 'googlemaps.html',tileDir + 'sample.txt'
            # Check if file exists then delete it #
        if os.path.isfile(oldG) or os.path.isfile(smp):
            os.remove(smp)
            os.remove(oldG)
            # Re-name the modified google doc. to the orig. name
            os.rename(tileDir + "googlemapsModified.html",tileDir + "googlemaps.html")
            print'modification completed and file renamed'
        else:    ## Show an error ##
            print "Error: {} OR {}  file not found".format(smp[len(smp)-10::1],oldG[len(oldG)-23::1])
    except IOError as e:
        print "I/O error({0}): {1}".format(e.errno, e.strerror) 

def joinfile(path,County_Name,originaldir):
    from shutil import move
    import os
    try:
        for i in os.listdir(path):
                # Remove Qouts from each file
            with open(path+i,'r') as f, open(path+i, 'w') as j:
                ff = f.read().replace('"',"'")
                j.write(ff)
                # Get a list of all files
        listFiles = [path+i for i in os.listdir(path)]
                # Join all files together seperated by comma
        with open(path +County_Name+'.txt','w') as p:
            for x in listFiles:
                with open(x) as c:
                    p.write(c.read()+",")
        with open(path+County_Name+'.txt','r') as t, open(path+County_Name+'.txt','w') as l:
            # Write correct format to text document
            tt = t.read()
            l.write('var counties = [  '+tt+'];')
                # Make corrected joined Txt doc. to JavaScript doc.
        os.rename( path+County_Name+'.txt',path+County_Name+'.js' )
                # Move JavaScript doc. to maptile directory
        move(path+County_Name+'.js',originaldir)
    except:
        print 'Changes could not be made...Check joinfile function'

if "__name__ "== "__main__": main()
