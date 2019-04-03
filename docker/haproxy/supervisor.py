#!/usr/bin/python3

from socket import socket
import subprocess as sp
from os import path

SERVER_ADDR = ('', 1500) # host to bind and port
CONF_PATH = '/usr/local/etc/haproxy/haproxy.cfg'
PID_FILE = '/run/haproxy.pid'

def reload_haproxy():
    """Reloads HAProxy or starts it. The reload is a soft reload and
    will cause an existing HAProxy instance to cleanly close any
    connections it is presently serving."""
    cmd = ['haproxy', '-D', '-f', CONF_PATH, '-p', '/run/haproxy.pid']

    try:
        with open(PID_FILE) as f:
            pids = f.read()
    except FileNotFoundError:
        pids = None

    cmd += ['-sf', pids] if pids is not None else []

    sp.run(cmd)

    print("Reloaded HAProxy.")

def handle_client(conn):
    """Copies whatever the clients sends us into the HAProxy config
    file, and reloads HAProxy.
    """
    with open(CONF_PATH, 'wb') as f:
        while True:
            body = conn.recv(4096)
            if not body:
                break
            f.write(body)
    reload_haproxy()
    conn.send("ok")

def loop(server):
    while True:
        conn, addr = server.accept()
        print("Connection received.")
        handle_client(conn)
        print("Connection closed.")
        conn.close()

def main():
    server = socket()
    server.bind(SERVER_ADDR)
    server.listen()
    loop(server)
    server.close()

if __name__ == '__main__':
    reload_haproxy()
    main()
