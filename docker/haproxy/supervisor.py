#!/usr/bin/python3

from socket import socket
import subprocess as sp
from os import path
import sys

SERVER_ADDR = ('', 1500) # host to bind and port
CONF_PATH = '/usr/local/etc/haproxy/haproxy.cfg'
PID_FILE = '/run/haproxy.pid'

def eprint(*args, **kwargs):
    print(*args, file=sys.stderr, **kwargs)

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

    eprint("Reloaded HAProxy.")

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
    conn.send("ok".encode('utf-8'))

def loop(server):
    while True:
        conn, addr = server.accept()
        eprint("Connection received.")
        handle_client(conn)
        eprint("Connection closed.")
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
