# Build resolve in the resolve-build build stage
FROM docker.grammatech.com/synthesis/sel as resolve-build
COPY . /root/quicklisp/local-projects/resolve
RUN cd /root/quicklisp/local-projects/resolve && make clean doc all

# Create the base resolve docker image.
# Copy in the resolve build, eliding any upstream projects to allow for
# them to be checked out fresh if this image is used in a CI pipeline
# or the project is rebuilt.
FROM docker.grammatech.com/synthesis/sel
RUN apt-get -y update && apt-get -y install jq
ENV PATH=/root/quicklisp/local-projects/resolve/bin:$PATH
COPY --from=resolve-build /root/quicklisp/local-projects/resolve /root/quicklisp/local-projects/resolve
WORKDIR /root/quicklisp/local-projects/resolve
