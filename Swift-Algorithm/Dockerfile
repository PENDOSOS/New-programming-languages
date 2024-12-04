FROM swift:latest

WORKDIR /app
COPY . /app
COPY config.txt /app/config.txt
RUN swift build
EXPOSE 8080

CMD ["./.build/debug/swift_algorithm"]