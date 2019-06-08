CREATE TABLE Authority (
    id BIGINT PRIMARY KEY
);

CREATE TABLE Project (
    id          BIGINT PRIMARY KEY,
    authorityid BIGINT NOT NULL,  

    CONSTRAINT project_authorityid_fkey 
        FOREIGN KEY (authorityid) 
        REFERENCES Authority (id)
);

CREATE TABLE Member (
    id            BIGINT    PRIMARY KEY,
    passwd        CHAR(128) NOT NULL,
    last_activity TIMESTAMP NOT NULL,
    is_leader     BOOLEAN   DEFAULT false NOT NULL
);

CREATE TABLE Action (
    id         BIGINT  PRIMARY KEY,
    is_support BOOLEAN NOT NULL,
    projectid  BIGINT  NOT NULL,
    memberid   BIGINT  NOT NULL,
    upvotes    BIGINT  DEFAULT 0 NOT NULL,
    downvlotes BIGINT  DEFAULT 0 NOT NULL,

    CONSTRAINT action_projectid_fkey
        FOREIGN KEY (projectid)
        REFERENCES Project (id),
    
    CONSTRAINT action_memberid_fkey
        FOREIGN KEY (memberid)
        REFERENCES Member (id)
);

CREATE TABLE Vote (
    memberid  BIGINT  NOT NULL,
    actionid  BIGINT  NOT NULL,
    is_upvote BOOLEAN NOT NULL,

    PRIMARY KEY (memberid, actionid),

    CONSTRAINT vote_memberid_fkey
        FOREIGN KEY (memberid)
        REFERENCES Member (id),

    CONSTRAINT vote_actionid_fkey
        FOREIGN KEY (actionid)
        REFERENCES Action (id)
);
