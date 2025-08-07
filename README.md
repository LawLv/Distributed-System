# Distributed Systems Projects

This repository contains five mini-projects that explore different core concepts in distributed systems, implemented using Erlang. Each project focuses on a specific protocol or system behavior, providing hands-on experience with concurrency, messaging, and fault tolerance.

---

## 📄 Projects Overview

### 1. Rudy: A Simple HTTP Server
Implements a basic multi-process HTTP server using Erlang's socket API. Includes request parsing, response generation, and benchmarking for performance under concurrency.

### 2. Routy: A Distributed Routing Protocol
Simulates dynamic message routing between networked nodes. Supports node addition/removal and maintains shortest-path routing using Dijkstra's algorithm.

### 3. Loggy: Logical Time Logger
Demonstrates how logical clocks (Lamport) maintain message order in distributed systems. Handles timestamped logging from multiple worker processes.

### 4. Groupy: Group Membership Service
Implements a group of processes with a designated leader. Supports leader election, consistent group view updates, and fault recovery after node crashes or joins.

### 5. Chordy: A Distributed Hash Table (DHT)
Implements a DHT based on the Chord protocol. Nodes form a ring structure and support key-based add/lookup operations, dynamic joining/leaving, and self-healing.

---

## 🛠️ Tech Stack

- Language: **Erlang**
- Focus: Distributed systems, message passing, concurrency
- Testing: Each project includes a test module to simulate scenarios and visualize behavior

---
