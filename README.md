# servant-crud

# Build

```
stack build
```

# Run

```
stack exec servant-crud-exe
```

# API

```
# get user list
curl -XGET 127.0.0.1:8081/users

# add new user
curl -XPOST 127.0.0.1:8081/users \
-H 'Content-type: application/json' \
-H 'Accept: application/json' \
-d '{"userFirstName":"Peter","userLastName":"Tchaikovsky","userRegistrationTimeStamp":"1840-05-07T00:00:00Z","userEmail":"peter@tchaiko.music.ru"}'

# get user by ID
curl -XGET 127.0.0.1:8081/users/1

# update user
curl -XPUT 127.0.0.1:8081/users/1 \
-H 'Content-type: application/json' \
-H 'Accept: application/json' \
-d '{"userFirstName":"New Isaac","userLastName":"New Newton","userRegistrationTimeStamp":"2024-03-01T00:00:00Z","userEmail":"new.isaac@newton.co.uk"}'

# delete user
curl -XDELETE 127.0.0.1:8081/users/1
```

# Build with Docker and create executable Docker image (WIP)

```
make all
```

# Run with Docker (WIP)

```
docker-compose up
```
