package com.rabbitmq.streams.harness;

import com.fourspaces.couchdb.Database;
import com.fourspaces.couchdb.Session;

public class CDB {

    public static final int couchDBPort = 5984;

    public final Session session;

    public final Database database;

    public CDB(final String host, final String dbName) {
        session = new Session(host, couchDBPort);
        database = session.getDatabase(dbName);
    }

    public CDB(final Session session, final Database database) {
        this.session = session;
        this.database = database;
    }

}
