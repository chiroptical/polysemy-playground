# polysemy-playground

[![Chiroptical](https://img.shields.io/badge/twitch.tv-chiroptical-purple?logo=twitch&style=for-the-badge)](https://twitch.tv/chiroptical)

I used https://github.com/EncodePanda/todo-rest as a reference for my implementation. They are quite similar. They use an in-memory key-value store whereas I use Beam. I also use `servant-generic` to ease integration of Polysemy and Beam.

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

Would like to use `co-log-polysemy` over `Polysemy.Trace` to have more powerful
control over our logging effect. However, we are waiting on
https://github.com/tathougies/beam/pull/443 for beam-migrate to work with stack
LTS-15.x.
