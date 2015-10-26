module CommonFiles(
 createURI,
 createFromWindowsPath,
 createFromPosixPath,
 getFileExtension
) where 

import qualified System.FilePath.Posix as P
import qualified System.FilePath.Windows as W

import Network.URI

import CommonStrings

createURI :: String -> String
createURI f@(x:':':ys) = createFromWindowsPath f
createURI f = createFromPosixPath f 
  
createFromWindowsPath f = 
 let 
  driver = W.takeDrive f
  directories = replaceString " " "/" (unwords (tail (W.splitDirectories f)))
 in  
  "file:///" ++ ( (head driver) : ":/" ) ++ directories 

createFromPosixPath f = "file://" ++ f

getFileExtension :: String -> String 
getFileExtension = P.takeExtension  
