#!/usr/bin/python3

import socket
from sys import stdout

SERVER_ADDR = ('', 80)

def handle_client(client):
    while True:
        bs = client.recv(4096)
        if not bs:
            break
        s = bs.decode('utf-8')
        stdout.write(s)

def loop(server):
    while True:
        conn, addr = server.accept()
        print("Connection received")
        try:
            handle_client(conn)
        except ConnectionResetError:
            print("Connection reset")
        conn.close()
        print("Connection closed")

def main():
    with socket.socket() as server:
        server.bind(SERVER_ADDR)
        server.listen()
        loop(server)
        server.close()

if __name__ == '__main__':
    main()
