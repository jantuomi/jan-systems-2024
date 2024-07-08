FROM alpine:latest

WORKDIR /app

# Install deps
RUN apk add --no-cache chicken inotify-tools bash pandoc coreutils
COPY requirements.list ./requirements.list
RUN chicken-install -from-list requirements.list

# Build app
COPY vendor ./vendor
COPY *.sh ./
COPY static ./static
COPY template.html *.scm ./

CMD ["/bin/bash", "monitor-inotifywait.sh"]
