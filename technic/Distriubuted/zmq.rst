============================
zmq Guide
============================

:author: yuqiao20@gmail.com
:version: 0.0.1
:Date: 12-01-24 09:09:28 

.. contents::

1 Basic Stuff
=============
Fixing the World
--------------------
zeroMQ:

- It's socket on steroids.
- It's like mailboxes with routing.
- It's fast.

some problems:

- Code has to talk code.
- Code has to be chatty, sociable, well-connected.
- Code has bo run like the human breain, trillios indvidual neurons firing off messages to each other.

To Fix the world:

- Solve the general problem of "how to connect any code to any cond, anywhere".
- Wrap that up in the simplest possible building blocks that people could understand and use easily.

ZeroMQ in hundred Words

    It looks like an embeddable networking library but acts like a concurrency framework. It gives you sockets that carry whole messages across various transports like in-process, inter-process, TCP and multicast. You can connect sockets N-to-N with patterns like fanout, pub-sub, task distribution, and request-reply. It's fast enough to be the fabric for clustered products. Its asynchronous I/O model gives you scalable multicore applicatons, built ad asynchronous
    message-processing tasks. ...

zmq doesn't know anything about the data you send but its size in bytes.

**zmq strings are length-specified, and are sent on the wire without a trailing null.**

Getting the Message Out
-----------------------
Note that When you use a SUB socket you **must** set a subscription using zmq_setsocketopt(3) and SUBSCRIBE, as in this code::

    sub_socket.setsockopt(zmq.SUBSCRIBE, sub_filter)

Even if you start a subscriber, wait a while, and then start the publisher, **te subscriber will always miss the first messages that the publisher sends.** This is because as the subsciber connects to the publisher, the publisher may already be sending message out.

Some points about the publish-subscribe pattern:

- A subscriber can in fact connect to more than one publisher.
- If a publisher has no connected subscriber, the it will simply drop all messages.
- If you're using TCP, and a subscriber is slow, messages will queue up on the publisher. We'll look at how to protect publishers agaist this using the "high-water mark" later.
- In the current versions of zmq, filtering happens at the subscriber side, not the publisher side.

Divied and Conquer
------------------

Programming with ZMQ
--------------------
- Learn ZMQ step by step. It's just on e simple API but it hides a world of possibilities. Take the possibilities slowly, master each one.
- Write nice code. Ugly code hides probelems and makes it hard for others to help you. 
- Test what you make as you make it. When your program doesn't work, you should know what five lines are to blame.
- When you find that things don't work as expected, break your code into pieces, test each one, see which one is not working. ZMQ lets you make essentially modular code, use that to your advantage.
- Make astractions (classes, methods, whaterver) as you need them. If you copy/paste a lot of code you're going to copy/paster errors too.

Getting the Contect Right
-------------------------
ZMQ applications always start by creating a congext, and using that for creating sockets.

The Context is the container for all sockets in a single process., and acts as the transport for inproc sockets, which are the fastest way to connect threads in one process.

If at runtime a process has two contexts, there are like seperate ZMQ instances, you shoul rememver:

    **Do one zmq_init(3) at the start of your main line code, and one zmq_term(3) at the end.**

Making a Clean Exit
-------------------
Always clean-up whenn you finish the job.
The ZMQ objects we need to worry about are messages, sockets, and contexts:

- Always close a message the moment you are done with it, usiing zmq_msg_close(3).
  If you are opening and closing a lot of sockets, that's probably a sig you need to redesigh your application.
  When you exit the program, close your sockets and then call zmq_term(3), this destroy th context.

When you use ZMQ in a language like Python, stuff get automatically freed for you.

Why We Needed ZMQ
-----------------
Any reusable messaging layer would need to solve all or most these:

- It handles I/O asynchronously, n background threads. These communicate with application threads using lock-free data structure, so zmq application need no locks, smaphores, or other wait states.
- Components can come and go dynamically and zmq will automatically reconnect.
- It queues messages automatically when needed.
- It has ways of dealing with over-full queues( called "high water mark"). When a queue is full, zmq automaticall blocks senders, or throws away messages, depending on the kind of messaging you are doing.
- It lets your app talk to each other over arbitracy transport: TCP, multicast, in-process, inter-process.
- It handles slow/blocked readers safely, using different strategies that depend on the messaging pattern.
- It lets you route messages using a variety of patterns such as request-reply adn publish-subscribe.
- It lets you place pattern-extending "devices" (small brokers) in the network when you need to reduce the complexity of interconnecting many pieces.
- It delivers whole messages exactly as they were sent, using a simple framing on the wire.
- It does not impose any format on messages.
- It handles network errors intelligently, Sometimes it retries, sometimes it tells you an operation failed.
- It reduces your carbon footprint.

2 intermediate stuff
====================
The Socket API
--------------
Sockets hvae a life in four parts, just like BSD socket:

- Creating and destroying sockets: zmq_socket, zmq_close.
- Configureing and checking sockets: zmq_setsockopt, zmq_getsockopt.
- Plugging sockets onto the network: zmq_bind, zmq_connect.
- Using sockets to carry data by writing and receiving messages on them: zmq_send, zmq_recv.

Plugging Sockets into the Topology
----------------------------------
ZMQ connections are somewhat different from old-fasioned TCP connections:

- The go across an arbitracy transpot.
- The exist when a client does zmq_connect to an endporint, whether or not a server has already done zmq_bind to that endpoit.
- They are asynchronous, and have queue that magically exixt where and when needed.
- They may express a cetain "messaging pattern", according to the type of socket used at each end.
- One socket may have many outgoing and many incoming connections.
- There is no zmq_accept method, when a socekt is bound to an endpoint it automatically starts accepting connections.
- Your application code cannot work with these connecions directly; the are encapculated under the socket.

Servers
    
    stable parts of your topology, with more-or-less fixed endpoint addresses.

Client:

    dynamic parts that come and go .

Using Sockets to Carry Data
---------------------------
Main differences between TCP sockets and zmq sockets when it comes and carrying data:

- zmq sockets carry messages, rather than bytes or frames. A message is a length-specified blob of binary data.
- zmq sockets do their I/O in a background thread. This means that messages arrive in a local input queue, and are sent from a local output queue, no matter what your application is busy doing. These are configurable memory queues, by the way.
- zmq sockets can, depending on the socket type, be connected to many other sockets. Where Tcp emulates a one-to-one phone call, zmq implements one-to-ma, many-to-many, many-to-one, and even one-to-one.
- zmq sockets can send to many endpoints, or receive from many endpoints.

The zmq_send method does not actually send the message to teh cocket connections. It queues the messages so the I/O thread can send it asynchronously.
If you create a message using zmq_msqinit_data you cannot reuse the data or free it., otherwise the I/O thread will rapidly find itself writing overwitten or unallocated garbage.

Unicast Transports
------------------
zmq profides a set of unicast transport (inproc, ipc, and tcp) and multicast transports(epgm, pgm).

tcp:

    A disconnected TCP transport. Elastic, portable and fast enough for most cases.

ipc:

    Inter-process transport. it does not work on Windows. You need to create these with appropriate permissions otherwise they may not bea shareable between processes running under different user ids.

inproc:
    The inter-thread transport. It is a connected signaling transport. The fastest. Limitition: **You must do bind before connect** . 


QMQ is Not a Neutral Carrier
----------------------------
zmq imposes a framing on the transport protocols it uses. The framing is not compatible with existing protocols, which tend to use their own framing.

I/O threads
-----------
One I/O thread is sufficient for all but the most extreme applications.

This is the magic '1' that we use when creating a context, meaning "use one I/O thread'::

    void *context = zmq_init(1);

A traditional networked application has one process or one thread per remote connection, and that process or thread handles one socket. zmq lets you collpase this entire structure into a single thread, and then break it up as necessary for scaling.

Core Messaging Patterns
-----------------------
Underneath the brown paper wrapping of ZMQ's socket API lies the world of messaging patterns.

ZMQ routes and queues messages according to precise recipes called *patterns* .

The built-in core ZMQ patterns are:

- Request-reply,
- Publish-subscribe,
- Pipeline,

other:

- Exclusive pair,

connect-bind pair:

- PUB, SUB
- REQ, REP
- REQ, ROUTER
- DEALER, REP
- DEALER, ROUTER
- DEALER, DEALER
- ROUTER, ROUTER
- PUSH, PULL
- PAIR, PAIR


Working with Messages
---------------------
**Note that when you hvae passed  a message to zmq_send, ZMQ will clear the message, i.e. set the size to zero. You cannot send the same message twice, and you can not access the message data after sending it.**

Some other things about messages:
- ZMQ sends and receives them automically. i.e. you get a whole message, or you don't get it at all.
- ZMQ dose not send a message right away but at some indeterminate later time.
- You can send zero-length messages, e.g. for sending a signal from one thread to another.
- A message must fit in memory. If you want to send files of arbitrary sizes, you should break them into pieces and send each piece as seperate message.
- You must call zmq_msg_close when finished with a message.

Handling Multiple Sockets
-------------------------
The right way is to use zmq_poll. An even better way might be to wrap zmq_poll in a framework that turns it into a nice event-drivern *reactor.* 

example::

    # Initialize poll set
    poller = zmq.Poller()
    poller.register(receiver, zmq.POLLIN)
    poller.register(subscriber, zmq.POLLIN)

    # Process messages from both sockets
    while True:
        socks = dict(poller.poll())

        if receiver in socks and socks[receiver] == zmq.POLLIN:
            message = receiver.recv()

Handling Errors and ETERM
-------------------------
ZMQ's error handling philosophy is a mix of fail-fast and resilience.

Processes, we believe, should be as vulnerable as possible to internal errors, adn as robust as possible against external attacks and errors.

Assertions, which pepper ther ZMQ code, are absolutely vital to robust code, they just have to be on the right side of the cellular wall.

Handling Interrrupt Signals
---------------------------
Realistic application need to shutdown cleanly when interrupted with Ctrl-C or another signal such as SIGTERM. By default, these simply kill the process,
meaning messages won't be flushed, files won't be closed cleanly, etc.

So we should handle a signal::

    interrupted = False

    def signal_handler(signum, frame):
        global interrupted
        interrupted = True
    
    signal.signal(signal.SIGINT, signal_handler)
    ...


Detecting Memory Leaks
----------------------
Using valgrind:

- install valgrind
- rebuild ZMQ with the ZMQ_MAKE_VALGRIND_HAPPY macro::

    $ export CPPFLAGS=-DZMQ_MAKE_VALGRIND_HAPPY
    $ ./configure
    $ make clean; make

- Fix your applications to exit cleanly after Ctrl-C.
- Build your application with -DDEBUG.
- Finally, run valgrind thus::

    valgrind --tool=memcheck --leak-check=full someprog

Multipart Messages
------------------
How we send the framse i a multipart message::

    zmq_send(socket, &message, ZMQ_SNDMORE);
    ...
    zmq_send(socket, &message, ZMQ_SNDMORE);
    ...
    zmq_send(socket, &message, 0);

How We receive it::

    zme_recv(socket, &message, 0);
    int64_t more;
    size_t more_size = sizeof(more);
    zmq_getsocketopt( socket, ZMQ_RCVMORE, &more, &more_size);
    if( !more)
        break;  // Last message part.

Intermediates and Devices
-------------------------
ZMQ devices can do intermediation of addresses, services, queues, or any other abstraction you care to define above the message and socket layers.

A Publish-Subscribe Proxy Server::

    # This is where the weather server sits
    frontend = context.socket(zmq.SUB)
    frontend.connect("tcp://192.168.55.210:5556")

    # This is our public endpoint for subscribers
    backend = context.socket(zmq.PUB)
    backend.bind("tcp://10.1.1.0:8100")

    # Subscribe on everything
    frontend.setsockopt(zmq.SUBSCRIBE, '') 

    # Shunt messages out to our own subscribers
    while True:
        # Process all parts of the message
        message = frontend.recv()
        more = frontend.getsockopt(zmq.RCVMORE)
        if more:
            backend.send(message, zmq.SNDMORE)
        else:
            backend.send(message)  # last message part 

A Request-Reply Broker::

    frontend = context.socket(zmq.XREP)
    backend = context.socket(zmq.XREQ)
    frontend.bind("tcp://*:5559")
    backend.bind("tcp://*:5560")

    # Initialize poll set
    poller = zmq.Poller()
    poller.register(frontend, zmq.POLLIN)
    poller.register(backend, zmq.POLLIN)

    # Switch messages between sockets
    while True:
        socks = dict(poller.poll())

        if socks.get(frontend) == zmq.POLLIN:
            message = frontend.recv()
            more = frontend.getsockopt(zmq.RCVMORE)
            if more:
                backend.send(message, zmq.SNDMORE)
            else:
                backend.send(message)

        if socks.get(backend) == zmq.POLLIN:
            message = backend.recv()
            more = backend.getsockopt(zmq.RCVMORE)
            if more:
                frontend.send(message, zmq.SNDMORE)
            else:
                frontend.send(message)

Built-in Devices:

- QUEUE, which is like the request-reply broker.
- FORWARDER, which is like the pub-sub proxy server.
- STREAMER, which is like FORWARDER but for pipeline flows.

The built-in devices do proper error handling. It's worth using the built-in devices when you can.

Example::

    context = zmq.Context(1)

    # Socket facing clients
    frontend = context.socket(zmq.XREP)
    frontend.bind("tcp://*:5559")

    # Socket facing services
    backend  = context.socket(zmq.XREQ)
    backend.bind("tcp://*:5560")

    zmq.device(zmq.QUEUE, frontend, backend)

    # We never get here...
    frontend.close()
    backend.close()
    context.term()

Mutlithreading with ZMQ
-----------------------
To make utterly perfect MT programs **we don't need mutexes, locks, or any other form of inter-thread communication except messages sent across ZMQ sockets.**

You should follow some rules to write happy multithreaded code with ZMQ:

- You MUST NOT access the same data from multiple threads. Using classic MT techniques like mutexes are an anti-pattern in ZMQ applications. The only exception is a ZMQ context object, with is threadsafe.
- You MUST create a ZMQ context for your process, and pass that to all threads that you want to connect via inproc sockets.
- You MAY treat threads as separate tasks, with their own context, but these threads cannot communicate over inproc.
- You MUST NOT share ZMQ sockets between threads.

Example::

    def worker_routine(worker_url, context):
        """ Worker routine """
        # Socket to talk to dispatcher
        socket = context.socket(zmq.REP)
        socket.connect(worker_url)
        while True:
            string  = socket.recv()
            print("Received request: [%s]\n" % (string))
            # do some 'work'
            time.sleep(1)
            #send reply back to client
            socket.send("World")

    def main():
        """ server routine """
        url_worker = "inproc://workers"
        url_client = "tcp://*:5555"
        # Prepare our context and sockets
        context = zmq.Context(1)
        # Socket to talk to clients
        clients = context.socket(zmq.XREP)
        clients.bind(url_client)
        # Socket to talk to workers
        workers = context.socket(zmq.XREQ)
        workers.bind(url_worker)
        # Launch pool of worker threads
        for i in range(5):
            thread = threading.Thread(target=worker_routine, args=(url_worker, context, ))
            thread.start()
        zmq.device(zmq.QUEUE, clients, workers)
        # We never get here but clean up anyhow
        clients.close()
        workers.close()
        context.term()

Signaling between Threads
-------------------------
How to coordinate your threads?

The Only mechanism that you should use are ZMQ messages.

Use pair sockets over the *inproc* transport::

    def step1(context):
        """ step1 """
        # Signal downstream to step 2
        sender = context.socket(zmq.PAIR)
        sender.connect("inproc://step2")
        sender.send("")

    def step2(context):
        """ step2 """
        # Bind to inproc: endpoint, then start upstream thread
        receiver = context.socket(zmq.PAIR)
        receiver.bind("inproc://step2")
        thread = threading.Thread(target=step1, args=(context, ))
        thread.start()
        # Wait for signal
        string = receiver.recv()
        # Signal downstream to step 3
        sender = context.socket(zmq.PAIR)
        sender.connect("inproc://step3")
        sender.send("")
        return

    def main():
        """ server routine """
        # Prepare our context and sockets
        context = zmq.Context(1)
        # Bind to inproc: endpoint, then start upstream thread
        receiver = context.socket(zmq.PAIR)
        receiver.bind("inproc://step3")
        thread = threading.Thread(target=step2, args=(context, ))
        thread.start()
        # Wait for signal
        string = receiver.recv()
        print("Test successful!\n")
        receiver.close()
        context.term()
        return

Classice pattern for mt with ZMQ:

1. Two thread communicate over inproc, using a shared context.
#. The parent thread creates one socket, binds it to  an inproc:// endpoint, and then starts the child thread, passing the context to it.
#. The child thread creates the second socket, connects it to that inproc:// endpoints, and then signals to the parent thread that it's ready.

Why use PAIR?

- PAIR has the advantage of refusing more then one connection, the pair is exclusive.
- ROUTER wraps your message in an "envelope", meaning your zero-size signal turns into a multipart message.

Node Coordination
-----------------
PAIR socket do not automatically reconnect if the remote node goes away and comes back.

Robust Model:

- Publisher opens PUB socket and starts sending "Hello" messages (not data).
- Subscribers connect SUB socket and wehn they receive a Hello Message the tell the pubilsher via a REQ/REP socket pair.
- When the publisher has had all the nessary confirmations, it starts to send real data.

Transient vs. Durable Sockets
-----------------------------
If a receiver(SUB, PULL, REQ) side a socket sets an identity, then the sending (PUB, PUSH, PULL) side will buffer messages when they aren't connected up to the HWM. The sending side does not need to set an identity for this to work.

Note that ZMQ's transmit and receive buffers are invisible and automatic, just like TCP's buffer are.

To create a durable socket::

    zmq_setsockopt(socket, ZMQ_IDENTITY, "LUCY", 4);

Some comments on setting a socket identity:

- If you want to set an identity you must do it **before** connecting or binding the socket.
- It's the receiver that sets an identity: It's kind of like a session cookie in an HTTP web application.
- Identities are binary strings.
- Do not use the same identity for more than one socket. Any socket trying to connnect using a identity already taken by another socket will just be disconnected.
- Do not use random identities in application that create lots of sockets.
- If you need to know the identity of the *peer* you got a message from, only the ROUTER socket does this for you automatically. For any other socket type you must send the address expicitly, as a message part.
- **Using durable socket is often a bad idea.** It make senders accumulate entropy, which makes architectures fragile.

**If you use durable sockets(i.e. if you set the identity on a SUB socket), you must also guard against queue explosion by using the high-warter makr or HWM, on the publisher socket.**

Pub-sub Message Envelopes
-------------------------
Putting the key into a separeate frame makes the matching very obvious.
Exmaple::

    ##### PUB PART ###############
    publisher = context.socket(zmq.PUB)
    publisher.bind("tcp://*:5563")

    while True:
        # Write two messages, each with an envelope and content
        publisher.send_multipart(["A", "We don't want to see this"])
        publisher.send_multipart(["B", "We would like to see this"])
        time.sleep(1)

    ##### SUB PART ###############
    subscriber = context.socket(zmq.SUB)
    subscriber.connect("tcp://localhost:5563")
    subscriber.setsockopt(zmq.SUBSCRIBE, "B")
    
    while True:
        # Read envelope with address
        [address, contents] = subscriber.recv_multipart()
        print("[%s] %s\n" % (address, contents))
    
A Bare Necessity
----------------
ZMQ is like a box of pieces that plug together.

3 Advanced Request-Reply Patterns
=================================
Request-Reply Envelopes
-----------------------
In the Request-Reply pattern, the envelope holds the return address for replies. It is how a ZMQ network with no state can create round-trip request-reply dialogs.

ZMQ implements envelops using multipart data.

patterns::

    [REQ] <--> [REP]
    [REQ] <--> [ROUTER--DEALER] <--> [REP]
    [REQ] <--> [ROUTER--DEALER] <--> [ROUTER--DEALER] <--> [REP]
    ...etc.

More detailed explanation of the foure socket types we use for request-reply patterns:

- DEALAR just load-balances the messages you send to aull connected peers.
- REQ prepends an empty message part to every message you send, and removes the empty message part from each message you receive.
- ROUTER prepends an envelope with reply address to each messageit receives, before passing it to the application. It also chop off the envelope (the first message part) from each message it sends, and users that reply address to decide which peer the message should go to.
- REP stores all the messagee part up to the first empty message part, when you receive  a message and it passes the rest( the data) to you application. When you send a reply, REP prepends the saved envelopes to the message and sents it back using the same semantics as ROUTER.

**REP requires that the envelopes end with an empty message part.** If you're not using REQ at the other end of the chain then you must add the empty message part yourself.

How dose ROUTER get the reply addresses from?
    
    It uses the cocket's identity.


Custom Request-Reply Routing
----------------------------
There are at least three routing patterns:

- Router-to-dealer.
- Router-to-mama(REQ).
- Router-to-papa(REP).

Some warnings about customer routing:

- This goes against a f fairly solid ZMQ rule: *delegate peer addressing to the socket.* The only reason we do it is because ZMQ lacks a wide range of routng algorithms.
- Future version ZMQ will properly do some of the routing we're build here.
- While the built-in routing has certain guarantees of scalability, such as be friendly to devices, custom routing doesn't, You will need to make your onw devices.








