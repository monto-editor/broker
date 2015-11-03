package monto.broker.websocket;

import org.apache.commons.cli.*;
import org.zeromq.ZContext;

import java.net.InetSocketAddress;

public class Main {

    private static Options options = new Options();
    private static HelpFormatter hf = new HelpFormatter();

    public static void main(String[] args) throws ParseException {
        options.addOption("c", true, "set zeromq source address")
                .addOption("k", true, "set zeromq sink address")
                .addOption("i", true, "set zeromq discovery address")
                .addOption("o", true, "set zeromq configuration address");

        String srcAddress = null;
        String snkAddress = null;
        String discAddress = null;
        String configAddress = null;

        CommandLineParser parser = new DefaultParser();
        CommandLine cmd = parser.parse(options, args);


        if (cmd.hasOption("c")) {
            srcAddress = cmd.getOptionValue("c");
        } else {
            exit();
        }

        if (cmd.hasOption("k")) {
            snkAddress = cmd.getOptionValue("k");
        } else {
            exit();
        }

        if (cmd.hasOption("i")) {
            discAddress = cmd.getOptionValue("i");
        } else {
            exit();
        }

        if (cmd.hasOption("o")) {
            configAddress = cmd.getOptionValue("o");
        } else {
            exit();
        }

        ZContext context = new ZContext(1);
        Thread wsd =  new Thread(new WebSocketRequestProxy(new InetSocketAddress(5006), discAddress, context));
        wsd.start();
        Thread wsc = new Thread(new WebSocketPublisherProxy(new InetSocketAddress(5008), configAddress, context));
        wsc.start();
        Thread wsp = new Thread(new WebSocketPublisherProxy(new InetSocketAddress(5002), srcAddress, context));
        wsp.start();
        WebSocketSubscriberProxy wss = new WebSocketSubscriberProxy(new InetSocketAddress(5003), snkAddress, context);
        wss.start();
    }

    private static void exit() {
        hf.printHelp("", "", options, "", true);
        System.exit(0);
    }
}
