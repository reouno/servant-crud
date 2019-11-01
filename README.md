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

# get user by ID
curl -XGET 127.0.0.1:8081/users/1

# update user

# delete user
curl -XDELETE 127.0.0.1:8081/users/1
```
