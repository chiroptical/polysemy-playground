# polysemy-playground

#### Select person/s

- All

```sh
curl http://localhost:8081/person
```

- Optional parameters (`name`, `age`, or `address`)

```sh
curl http://localhost:8081/person?name=...&age=...&address=...
```

#### Insert person

```sh
curl -H "Content-type: application/json" \
     -d '{"name": "Barry", "age": 31, "address": "Somewhere"}' \
     -X POST http://localhost:8081/person
```

#### Update person

```sh
curl -H "Content-type: application/json" \
     -d '{"id": 1, "name": "Barry", "age": 31, "address": "Somewhere"}' \
     -X PUT http://localhost:8081/person
```

#### Delete person

```sh
curl -H "Content-type: application/json" \
     -d '{"id": 1, "name": "Barry", "age": 31, "address": "Somewhere"}' \
     -X DELETE http://localhost:8081/person
```

#### Notes

Would like to use co-log-polysemy to have more powerful control over our trace
effect. However, we are waiting on https://github.com/tathougies/beam/pull/443
for beam-migrate to work with stack LTS-15.x.
