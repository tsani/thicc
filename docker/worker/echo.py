#!/usr/bin/python3

import socket
from sys import stderr

SERVER_ADDR = ('', 80)

def handle_client(client):
    while True:
        bs = client.recv(4096)
        if not bs:
            break
        client.send(bs)
        s = bs.decode('utf-8')
        stderr.write(s)

def loop(server):
    while True:
        conn, addr = server.accept()
        print("Connection received", file=stderr)
        try:
            handle_client(conn)
        except ConnectionResetError:
            print("Connection reset", file=stderr)
        conn.close()
        print("Connection closed", file=stderr)

def main():
    with socket.socket() as server:
        server.bind(SERVER_ADDR)
        server.listen()
        loop(server)
        server.close()

if __name__ == '__main__':
    main()
