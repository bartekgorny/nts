--
-- Name: device_01; Type: TABLE; Schema: public; Owner: nts
--

CREATE TABLE device_01 (
    id bigint NOT NULL,
    dtm timestamp without time zone,
    coords point,
    data text,
    hex boolean,
    frame text,
    received timestamp without time zone,
    internal text
);

ALTER TABLE device_01 OWNER TO nts;

CREATE SEQUENCE device_01_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER TABLE device_01_id_seq OWNER TO nts;

ALTER SEQUENCE device_01_id_seq OWNED BY device_01.id;

ALTER TABLE ONLY device_01 ALTER COLUMN id SET DEFAULT nextval('device_01_id_seq'::regclass);

CREATE INDEX device_01_dtm ON device_01 USING btree (dtm);

CREATE INDEX device_01_rec ON device_01 USING btree (received);



