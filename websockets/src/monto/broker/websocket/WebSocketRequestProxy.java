package monto.broker.websocket;

import org.java_websocket.WebSocket;
import org.java_websocket.handshake.ClientHandshake;
import org.java_websocket.server.WebSocketServer;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMQ.Socket;

import java.net.InetSocketAddress;

public class WebSocketRequestProxy extends WebSocketServer {

    private ZContext context;
    private Socket socket;

    public WebSocketRequestProxy(InetSocketAddress webSocketAddress,String zmqAddress, ZContext context) {
        super(webSocketAddress);
        this.context = context;
        connect(zmqAddress);
    }

    private void connect(String address) {
        socket = context.createSocket(ZMQ.REQ);
        socket.connect(address);
    }

    @Override
    public void onOpen(WebSocket webSocket, ClientHandshake clientHandshake) {

    }

    @Override
    public void onClose(WebSocket webSocket, int i, String s, boolean b) {

    }

    @Override
    public void onMessage(WebSocket webSocket, String s) {
        System.out.println("websocket -> zmq");
        socket.send(s);
        String msg = socket.recvStr();
        webSocket.send(msg);
        System.out.println("zmq -> websocket");
    }

    @Override
    public void onError(WebSocket webSocket, Exception e) {
        e.printStackTrace();
    }
}
