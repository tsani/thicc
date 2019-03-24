FROM haproxy:1.7
COPY base_haproxy.cfg /usr/local/etc/haproxy/haproxy.cfg
RUN useradd -r haproxy
RUN apt-get update
RUN apt-get install -y netcat
RUN apt-get install -y python3
COPY supervisor.py /
ENTRYPOINT ["./supervisor.py"]
