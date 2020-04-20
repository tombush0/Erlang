# Homework

- [pollution_tests](../pollution_server/src/pollution_tests)
- [pollution_server](../pollution_server/src/pollution_server)
- [pollution_server_tests](../pollution_server/src/pollution_server_tests)


# Raport

System: Windows 10

Cores: 4

## People: 1000, Lockers: 1000

```erlang
parcels:test(1000, 1000).
```
```erlang
Sequential took 0.406 seconds
Each one took 0.171999 seconds
Devided took 0.140999 seconds
```

## People: 1000, Lockers: 10000
```erlang
parcels:test(1000, 10000).
```
```erlang
Sequential took 5.063 seconds
Each one took 2.389999 seconds
Devided took 1.656999 seconds
```
## People: 1000, Lockers: 100000

```erlang
parcels:test(1000, 100000).
```
```erlang
Sequential took 37.984 seconds
Each one took 22.077999 seconds
Devided took 13.812999 seconds
```
## People: 1000, Lockers: 1000000
```erlang
parcels:test(1000, 1000000).
```
```erlang
Sequential took 338.656 seconds
Each one crashed, too much to allocate
Devided took 136.875 seconds
```
## People: 10000, Lockers: 1000

```erlang
parcels:test(10000, 1000).
```
```erlang
Sequential took 4.297 seconds
Each one took 1.577999 seconds
Devided took 1.436999 seconds
```
## People: 100000, Lockers: 1000
```erlang
parcels:test(100000, 1000).
```
```erlang
Sequential took 40.484 seconds
Each one took 18.390999 seconds
Devided took 13.344 seconds
```
## People: 10000, Lockers: 10000
```erlang
parcels:test(10000, 10000).
```
```erlang
Sequential took 34.828 seconds
Each one took 14.296999 seconds
Devided took 8.343999 seconds
```
## People: 100000, Lockers: 100000
```erlang
parcels:test(100000, 100000).
```
```erlang
Devided took 1138.14 seconds
Rest was taking too long to be bothered
```
