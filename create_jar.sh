#!/bin/sh

tempFolderName="temp"
projectName="Risk"
lowercaseProjectName=$(echo $projectName | tr '[:upper:]' '[:lower:]')

mill Risk.ui.fullOpt
mill Risk.backend.assembly

cd out/$projectName
echo $tempFolderName
rm -rf $tempFolderName
mkdir $tempFolderName
unzip backend/assembly/dest/out.jar -d ./$tempFolderName/
cp ui/fullOpt/dest/out.js ./$tempFolderName/gbge/ui/generatedJSFiles
cd $tempFolderName
zip -r out .
mv out.zip ../$lowercaseProjectName.jar