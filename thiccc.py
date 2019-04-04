import requests
import json
import optparse
import sys

host = 'http://localhost:7133/services/'
args = sys.argv
length = leng(args)


if length < 2:
    badInput()

command.toLower() = args[1]

if command == "?":
    print("Command options include:\ncreate <service-name> <command>\nscale
        <service-name> <total-workers>\ndelete <service-name>")

elif command == "create":
    #check right # args
    if length < 3:
        badInput()

    serviceName = args[2]
    containerCommand = args[3:]

    createService(serviceName, containerCommand)

elif command == "delete":
    #check right # args
    if length != 3:
        badInput()

    serviceName = args[2]

    deleteService(serviceName)


elif command == "scale":
    #check right # args
    if length != 4:
        badInput()

    serviceName = args[2]
    numWorkers = args[3]

    scaleService(serviceName, numWorkers)

else:
    badInput()

#create service
def createService(serviceName, command):
    payload = {"command":command}
    r = requests.put(host + serviceName, payload)

    print(r.json)


#scale service
def scaleService(serviceName, numberOfWorkers):
    num = 0
    try:
        num = int(numberOfWorkers)
    except e:
        print("Error: Expected an integer for number of workers, command failed")
        exit(1)

    payload = {"number":num}
    r = requests.post(host + serviceName, payload)

    print(r.json)


    #delete service
def deleteService(serviceName):
    r = requests.delete(host + serviceName)
    print(r.json)

#if we get unexpected input
def badInput():
    print("Error: enter a valid command. Execute \'thiccc ?\' for help")
    exit(1)
