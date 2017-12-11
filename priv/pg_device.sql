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

ALTER TABLE ONLY device_01
    ADD CONSTRAINT device_01_key UNIQUE (id);

CREATE INDEX device_01_id ON device_01 USING btree (id);

CREATE INDEX device_01_dtm ON device_01 USING btree (dtm);

CREATE INDEX device_01_rec ON device_01 USING btree (received);



