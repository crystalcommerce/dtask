# DTask

Distributed task executor.

## About

DTask is an OTP application that provides distributed tasks
execution. Tasks are registered with DTask using a similar interface
to Erlang's timer module as well as a collection of work nodes where
the tasks are to be executed. DTask will run the scheduled tasks
evenly distributed across the worker nodes.
