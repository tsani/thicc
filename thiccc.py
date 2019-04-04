#!/usr/bin/env python

import requests
import json
import optparse
import sys

host = 'http://localhost:7133/'
args = sys.argv
length = len(args)


if length < 2:
    badInput()

command = args[1].lower()

def main():
    if command == "?":
        print("Command options include:\ncreate <service-name> <command>\nscale <service-name> <total-workers>\ndelete <service-name>")

    if length < 3:
        badInput()

    elif command == "service":
        nextCommand = args[2].lower()

        elif nextCommand == "create"

            #check right # args
            if length < 4:
                badInput()
            serviceName = args[3]
            containerCommand = args[4:]
            createService(serviceName, containerCommand)

        elif nextCommand == "delete":
                #check right # args
            if length != 4:
                badInput()
            serviceName = args[3]
            deleteService(serviceName)

        elif nextCommand == "scale":
            #check right # args
            if length != 5:
                badInput()

            serviceName = args[3]
            numWorkers = args[4]

            scaleService(serviceName, numWorkers)
    elif command == "blob":
        if length < 4:
            badInput()

        nextCommand = args[2].lower()

        if nextCommand = "create":
            if length != 5:
                badInput()

            createBlob(args[3],args[4])

        elif nextCommand = "delete":
            if length != 4:
                badInput()

            deleteBlob(args[3])

        else:
            badInput()

    else:
        badInput()

def send(verb, endpoint, payload=None):
    f = getattr(requests, verb)
    if payload is None:
        return f(host + endpoint)

    return f(
        host + endpoint,
        json.dumps(payload),
        headers={
            "Content-Type": "application/json"
        }
    )

#create service
def createService(serviceName, command):
    print("createService")
    payload = {"command":command}
    r = send('put', 'services/' + serviceName, payload)
    showResponse(r)


#scale service
def scaleService(serviceName, numberOfWorkers):
    num = 0
    try:
        num = int(numberOfWorkers)
    except e:
        print("Error: Expected an integer for number of workers, command failed")
        exit(1)

    payload = {"number":num}
    r = send('post', 'services/' + serviceName, payload)

    showResponse(r)

    #delete service
def deleteService(serviceName):
    r = send('delete', 'services/' + serviceName)
    showResponse(r)

def showResponse(r):
    print('success' if r.text == '' else r.json())

def createBlob(blobName, path):
    #read file
    try:
        fp = open(path,"r")
        payload = fp.read()
    except e:
        print(e)
        exit(1)

    r = send('put','blob/' + blobName, payload)
    showResponse(r)

def deleteBlob(blobName):
    r = send('delete', 'blob/' + blobName)
    showResponse(r)

#if we get unexpected input
def badInput():
    print("Error: enter a valid command. Execute \'thiccc ?\' for help")
    exit(1)

if __name__ == '__main__':
    main()
