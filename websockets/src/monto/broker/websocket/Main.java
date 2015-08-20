package monto.broker.websocket;

import org.zeromq.ZContext;

import java.net.InetSocketAddress;

public class Main {

    public static void main(String[] args) {
        ZContext context = new ZContext(1);
        WebSocketPublisherProxy wsp = new WebSocketPublisherProxy(new InetSocketAddress(5002), "tcp://*:5000", context);
        wsp.start();
        WebSocketSubscriberProxy wss = new WebSocketSubscriberProxy(new InetSocketAddress(5003), "tcp://*:5001", context);
        wss.start();
    }
}
