# reflection about the position of the directory in the installation

FOLDER_LOCATION = File.dirname(__FILE__)
# we are in the src/ folder; go up
INSTALLATION_LOCATION = File.split(FOLDER_LOCATION)[0]
