INSERT INTO Member VALUES
    (?, (SELECT crypt(?, gen_salt('md5'))), to_timestamp(?), ?);