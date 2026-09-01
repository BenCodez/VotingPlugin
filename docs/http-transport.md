# Secure HTTP proxy transport

The `HTTP` bungee method gives every backend an outbound encrypted connection to one HTTPS listener on the proxy. Only the proxy port is opened; backend servers need no inbound transport ports.

## Quick setup

1. On the proxy, set the following in `bungeeconfig.yml`:

   ```yaml
   BungeeMethod: HTTP
   HTTP:
     Host: '0.0.0.0'
     Port: 1297
     PublicEndpoint: 'https://proxy.example.com:1297/'
   ```

2. Allow TCP port `1297` to the proxy. `PublicEndpoint` must resolve directly to this VotingPlugin listener.
3. Restart the proxy and run `/votingpluginbungee httpcode <server>`. The name must exactly identify the intended backend; generate a separate code for each backend.
4. On a backend, set a unique `Server`, enable bungee mode, select `HTTP`, and paste its code into `BungeeSettings.yml`:

   ```yaml
   UseBungeecord: true
   Server: lobby-1
   BungeeMethod: HTTP
   HTTP:
     ConnectionCode: 'paste-code-here'
   ```

5. Restart the backend. Once enrollment succeeds, remove `ConnectionCode` from the configuration. The backend's private identity is stored in its VotingPlugin data folder and is reused automatically. A different nonblank code is treated as an explicit re-enrollment request; a digest lets harmless restarts recognize the already-consumed code without storing its secret token.

Connection codes expire after 15 minutes and can be used only once. Treat a fresh code like a temporary password: transfer it privately and do not publish it in logs, tickets, or chat rooms.

## Security model

- TLS keys and a private certificate authority are generated automatically on the proxy. No shared transport password or public CA setup is required.
- Enrollment pins both the exact proxy certificate and its private authority. Normal traffic trusts only that pinned private authority and keeps HTTPS hostname verification enabled, allowing the proxy leaf certificate to rotate safely without trusting public certificate authorities. It uses a distinct client certificate bound to that backend's canonical `Server` name.
- Proxy and backend leaf certificates renew automatically during a 30-day pre-expiry window. Backend renewal is authenticated by the still-valid mTLS identity, persisted before use, and switches the proxy binding only after the replacement successfully connects; no new connection code is needed.
- Every normal request is authenticated again at the application boundary. A payload cannot claim another backend identity, and redirect following is disabled.
- TLS 1.3 is required and weak protocols are disabled.
- Enrollment tokens are 256-bit random capabilities, single-use, short-lived, and retained by the proxy only as hashes.
- Credentials, keys, pins, and revocation state are written atomically with owner-only permissions where the operating system supports them.
- Request bodies, queues, batches, worker pools, concurrent requests, per-backend polling, and request rates are bounded. Exact paths, methods, and JSON content types are enforced.
- Proxy-to-backend messages stay in an owner-only durable proxy queue until the backend acknowledgement is durably applied. Before a backend callback can change vote or reward state, the backend fsyncs that delivery ID in its credential directory; a restart therefore acknowledges a replay without awarding it twice. The fence is deleted only after a matching authenticated HTTP 200 confirms the proxy removed the queued message.
- This crash fence deliberately favors preventing duplicate rewards: a process or host failure in the very small interval after the fence is persisted but before the callback begins can suppress that callback after restart. Fully transactional exactly-once reward execution is not possible because arbitrary reward commands and external plugin effects cannot share a transaction with the transport journal.
- Backend-to-proxy messages retain the existing bounded in-process retry semantics. Vote caching remains responsible for application-level durability in that direction.

The listener must terminate TLS itself because client-certificate authentication is part of the protocol. Do not put an HTTP TLS-terminating reverse proxy or CDN in front of it. A TCP/L4 proxy that passes TLS through unchanged is suitable. Internet-facing installations should also use the host firewall or provider firewall for volumetric denial-of-service protection; an application cannot fully absorb a link or TCP flood.

If a backend host or its private credential is compromised, run `/votingpluginbungee httprevoke <server>` on the proxy before generating a new connection code. Revocation takes effect on the next request and permits a replacement identity to enroll under that server name. Keep the proxy's `http` data directory backed up and private: it contains the transport authority.

## Performance

The connector reuses HTTP/1.1 TLS connections, batches messages and acknowledgements, and performs all network, certificate, and delivery-journal work off the game thread. A two-second bounded long poll avoids busy polling while limiting the worst-case delay for a backend message queued just after an idle request began. Callback lanes are single-worker FIFO queues; when their fixed capacity is reached, bounded listener/poller workers apply backpressure instead of dropping an older callback and admitting newer work out of order. All queues and admission pools remain bounded to prevent traffic bursts from causing unbounded memory growth.
