#!/usr/bin/env python

import requests
import json
import optparse
import sys

host = 'http://localhost:7133/services/'
args = sys.argv
length = len(args)


if length < 2:
    badInput()

command = args[1].lower()

def main():
    if command == "?":
        print("Command options include:\ncreate <service-name> <command>\nscale <service-name> <total-workers>\ndelete <service-name>")
        
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
    r = send('put', serviceName, payload)
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
    r = send('post', serviceName, payload)
    showResponse(r)

    #delete service
def deleteService(serviceName):
    r = send('delete', serviceName)
    showResponse(r)

def showResponse(r):
    print('success' if r.text == '' else r.json())

#if we get unexpected input
def badInput():
    print("Error: enter a valid command. Execute \'thiccc ?\' for help")
    exit(1)

if __name__ == '__main__':
    main()
