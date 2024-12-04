# Use a base image with Lua installed, such as Alpine Linux
FROM alpine:latest

# Install Lua and LuaRocks package manager
RUN apk --update add lua5.3 lua5.3-dev luarocks

# Create a working directory for your Lua application
WORKDIR /app

# Copy your Lua application files into the container
COPY . .

# Define the command to run your Lua application
CMD ["/usr/bin/lua5.3", "main.lua"]