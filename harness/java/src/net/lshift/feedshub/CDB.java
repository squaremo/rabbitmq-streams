package net.lshift.feedshub;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
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

	private CDB(final Session session, final Database database) {
		this.session = session;
		this.database = database;
	}

	public static CDB constructPrivateCDB(final Session session)
			throws NoSuchAlgorithmException {
		// TODO how much random do we need here?
		SecureRandom random = new SecureRandom();
		String dbName = null;
		byte[] randomBytes = new byte[8];
		MessageDigest md = MessageDigest.getInstance("SHA");
		StringBuilder hashStringBuilder = new StringBuilder();
		do {
			random.nextBytes(randomBytes);
			md.reset();
			md.update(randomBytes);
			byte[] hashBytes = md.digest();
			hashStringBuilder.setLength(0);
			for (int idx = 0; idx < hashBytes.length; ++idx) {
				hashStringBuilder.append(Integer
						.toHexString(0xFF & hashBytes[idx]));
			}
			dbName = hashStringBuilder.toString();

		} while (session.getDatabaseNames().contains(dbName)
				|| hashStringBuilder.charAt(0) < 'a'); // start with a char, see couchdb api

		return new CDB(session, session.createDatabase(dbName));
	}

}
