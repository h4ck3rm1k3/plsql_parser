// $ANTLR 3.2 debian-14 PLSQL3.g 2016-10-07 06:53:01

import org.antlr.runtime.*;
import java.util.Stack;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
public class PLSQL3Lexer extends Lexer {
    public static final int T__144=144;
    public static final int T__143=143;
    public static final int T__146=146;
    public static final int T__50=50;
    public static final int T__145=145;
    public static final int T__140=140;
    public static final int CHARSET_ATTR=18;
    public static final int T__142=142;
    public static final int T__141=141;
    public static final int MINUS=14;
    public static final int ROWCOUNT_ATTR=28;
    public static final int TYPE_ATTR=19;
    public static final int T__59=59;
    public static final int T__55=55;
    public static final int T__56=56;
    public static final int T__57=57;
    public static final int T__58=58;
    public static final int ID=40;
    public static final int T__51=51;
    public static final int T__137=137;
    public static final int T__52=52;
    public static final int T__136=136;
    public static final int T__53=53;
    public static final int T__139=139;
    public static final int T__54=54;
    public static final int T__138=138;
    public static final int T__133=133;
    public static final int T__132=132;
    public static final int T__60=60;
    public static final int T__135=135;
    public static final int NOT_EQ=34;
    public static final int T__61=61;
    public static final int T__134=134;
    public static final int T__131=131;
    public static final int T__130=130;
    public static final int DOT=5;
    public static final int LPAREN=7;
    public static final int FOUND_ATTR=25;
    public static final int GEQ=36;
    public static final int LBRACK=23;
    public static final int T__66=66;
    public static final int T__67=67;
    public static final int T__129=129;
    public static final int T__68=68;
    public static final int T__69=69;
    public static final int T__62=62;
    public static final int T__126=126;
    public static final int T__63=63;
    public static final int T__125=125;
    public static final int T__64=64;
    public static final int T__128=128;
    public static final int T__65=65;
    public static final int T__127=127;
    public static final int T__166=166;
    public static final int T__165=165;
    public static final int T__167=167;
    public static final int T__162=162;
    public static final int T__161=161;
    public static final int T__164=164;
    public static final int T__163=163;
    public static final int SEMI=4;
    public static final int T__160=160;
    public static final int ASSIGN=6;
    public static final int N=45;
    public static final int DIVIDE=31;
    public static final int ARROW=39;
    public static final int BULK_ROWCOUNT_ATTR=29;
    public static final int T__159=159;
    public static final int T__158=158;
    public static final int T__155=155;
    public static final int COMMA=12;
    public static final int T__154=154;
    public static final int T__157=157;
    public static final int T__156=156;
    public static final int T__151=151;
    public static final int AT_SIGN=33;
    public static final int T__150=150;
    public static final int T__153=153;
    public static final int T__152=152;
    public static final int ASTERISK=21;
    public static final int ROWTYPE_ATTR=20;
    public static final int PERCENTAGE=43;
    public static final int DOUBLEVERTBAR=30;
    public static final int SL_COMMENT=48;
    public static final int EXPONENT=32;
    public static final int T__148=148;
    public static final int T__147=147;
    public static final int T__149=149;
    public static final int T__91=91;
    public static final int T__100=100;
    public static final int T__92=92;
    public static final int T__93=93;
    public static final int T__102=102;
    public static final int T__94=94;
    public static final int T__101=101;
    public static final int T__90=90;
    public static final int T__99=99;
    public static final int T__95=95;
    public static final int T__96=96;
    public static final int T__97=97;
    public static final int T__98=98;
    public static final int NUMBER=15;
    public static final int RPAREN=8;
    public static final int EQ=22;
    public static final int RBRACK=24;
    public static final int DOUBLEDOT=9;
    public static final int PLUS=13;
    public static final int T__122=122;
    public static final int T__70=70;
    public static final int T__121=121;
    public static final int LTH=37;
    public static final int QUOTE=46;
    public static final int T__71=71;
    public static final int T__124=124;
    public static final int T__72=72;
    public static final int T__123=123;
    public static final int T__120=120;
    public static final int T__77=77;
    public static final int T__119=119;
    public static final int ISOPEN_ATTR=27;
    public static final int T__78=78;
    public static final int T__118=118;
    public static final int T__79=79;
    public static final int T__73=73;
    public static final int T__115=115;
    public static final int WS=47;
    public static final int EOF=-1;
    public static final int T__74=74;
    public static final int T__114=114;
    public static final int T__75=75;
    public static final int T__117=117;
    public static final int T__76=76;
    public static final int T__116=116;
    public static final int T__80=80;
    public static final int T__111=111;
    public static final int T__81=81;
    public static final int T__110=110;
    public static final int T__82=82;
    public static final int T__113=113;
    public static final int T__83=83;
    public static final int T__112=112;
    public static final int VERTBAR=44;
    public static final int NOTFOUND_ATTR=26;
    public static final int COLON=17;
    public static final int LLABEL=10;
    public static final int ML_COMMENT=49;
    public static final int T__88=88;
    public static final int T__108=108;
    public static final int RLABEL=11;
    public static final int T__89=89;
    public static final int T__107=107;
    public static final int LEQ=38;
    public static final int GTH=35;
    public static final int T__109=109;
    public static final int T__84=84;
    public static final int T__104=104;
    public static final int POINT=42;
    public static final int T__85=85;
    public static final int T__103=103;
    public static final int DOUBLEQUOTED_STRING=41;
    public static final int T__86=86;
    public static final int T__106=106;
    public static final int T__87=87;
    public static final int T__105=105;
    public static final int QUOTED_STRING=16;

    // delegates
    // delegators

    public PLSQL3Lexer() {;} 
    public PLSQL3Lexer(CharStream input) {
        this(input, new RecognizerSharedState());
    }
    public PLSQL3Lexer(CharStream input, RecognizerSharedState state) {
        super(input,state);

    }
    public String getGrammarFileName() { return "PLSQL3.g"; }

    // $ANTLR start "T__50"
    public final void mT__50() throws RecognitionException {
        try {
            int _type = T__50;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:7:7: ( 'CREATE' )
            // PLSQL3.g:7:9: 'CREATE'
            {
            match("CREATE"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__50"

    // $ANTLR start "T__51"
    public final void mT__51() throws RecognitionException {
        try {
            int _type = T__51;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:8:7: ( 'OR' )
            // PLSQL3.g:8:9: 'OR'
            {
            match("OR"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__51"

    // $ANTLR start "T__52"
    public final void mT__52() throws RecognitionException {
        try {
            int _type = T__52;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:9:7: ( 'IS' )
            // PLSQL3.g:9:9: 'IS'
            {
            match("IS"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__52"

    // $ANTLR start "T__53"
    public final void mT__53() throws RecognitionException {
        try {
            int _type = T__53;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:10:7: ( 'AS' )
            // PLSQL3.g:10:9: 'AS'
            {
            match("AS"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__53"

    // $ANTLR start "T__54"
    public final void mT__54() throws RecognitionException {
        try {
            int _type = T__54;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:11:7: ( 'END' )
            // PLSQL3.g:11:9: 'END'
            {
            match("END"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__54"

    // $ANTLR start "T__55"
    public final void mT__55() throws RecognitionException {
        try {
            int _type = T__55;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:12:7: ( 'BEGIN' )
            // PLSQL3.g:12:9: 'BEGIN'
            {
            match("BEGIN"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__55"

    // $ANTLR start "T__56"
    public final void mT__56() throws RecognitionException {
        try {
            int _type = T__56;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:13:7: ( 'CONSTANT' )
            // PLSQL3.g:13:9: 'CONSTANT'
            {
            match("CONSTANT"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__56"

    // $ANTLR start "T__57"
    public final void mT__57() throws RecognitionException {
        try {
            int _type = T__57;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:14:7: ( 'NOT' )
            // PLSQL3.g:14:9: 'NOT'
            {
            match("NOT"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__57"

    // $ANTLR start "T__58"
    public final void mT__58() throws RecognitionException {
        try {
            int _type = T__58;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:15:7: ( 'NULL' )
            // PLSQL3.g:15:9: 'NULL'
            {
            match("NULL"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__58"

    // $ANTLR start "T__59"
    public final void mT__59() throws RecognitionException {
        try {
            int _type = T__59;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:16:7: ( 'DEFAULT' )
            // PLSQL3.g:16:9: 'DEFAULT'
            {
            match("DEFAULT"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__59"

    // $ANTLR start "T__60"
    public final void mT__60() throws RecognitionException {
        try {
            int _type = T__60;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:17:7: ( 'DECLARE' )
            // PLSQL3.g:17:9: 'DECLARE'
            {
            match("DECLARE"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__60"

    // $ANTLR start "T__61"
    public final void mT__61() throws RecognitionException {
        try {
            int _type = T__61;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:18:7: ( 'EXCEPTION' )
            // PLSQL3.g:18:9: 'EXCEPTION'
            {
            match("EXCEPTION"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__61"

    // $ANTLR start "T__62"
    public final void mT__62() throws RecognitionException {
        try {
            int _type = T__62;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:19:7: ( 'GOTO' )
            // PLSQL3.g:19:9: 'GOTO'
            {
            match("GOTO"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__62"

    // $ANTLR start "T__63"
    public final void mT__63() throws RecognitionException {
        try {
            int _type = T__63;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:20:7: ( 'WHEN' )
            // PLSQL3.g:20:9: 'WHEN'
            {
            match("WHEN"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__63"

    // $ANTLR start "T__64"
    public final void mT__64() throws RecognitionException {
        try {
            int _type = T__64;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:21:7: ( 'BINARY_INTEGER' )
            // PLSQL3.g:21:9: 'BINARY_INTEGER'
            {
            match("BINARY_INTEGER"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__64"

    // $ANTLR start "T__65"
    public final void mT__65() throws RecognitionException {
        try {
            int _type = T__65;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:22:7: ( 'BINARY_FLOAT' )
            // PLSQL3.g:22:9: 'BINARY_FLOAT'
            {
            match("BINARY_FLOAT"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__65"

    // $ANTLR start "T__66"
    public final void mT__66() throws RecognitionException {
        try {
            int _type = T__66;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:23:7: ( 'BINARY_DOUBLE' )
            // PLSQL3.g:23:9: 'BINARY_DOUBLE'
            {
            match("BINARY_DOUBLE"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__66"

    // $ANTLR start "T__67"
    public final void mT__67() throws RecognitionException {
        try {
            int _type = T__67;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:24:7: ( 'NATURAL' )
            // PLSQL3.g:24:9: 'NATURAL'
            {
            match("NATURAL"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__67"

    // $ANTLR start "T__68"
    public final void mT__68() throws RecognitionException {
        try {
            int _type = T__68;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:25:7: ( 'POSITIVE' )
            // PLSQL3.g:25:9: 'POSITIVE'
            {
            match("POSITIVE"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__68"

    // $ANTLR start "T__69"
    public final void mT__69() throws RecognitionException {
        try {
            int _type = T__69;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:26:7: ( 'NUMBER' )
            // PLSQL3.g:26:9: 'NUMBER'
            {
            match("NUMBER"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__69"

    // $ANTLR start "T__70"
    public final void mT__70() throws RecognitionException {
        try {
            int _type = T__70;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:27:7: ( 'NUMERIC' )
            // PLSQL3.g:27:9: 'NUMERIC'
            {
            match("NUMERIC"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__70"

    // $ANTLR start "T__71"
    public final void mT__71() throws RecognitionException {
        try {
            int _type = T__71;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:28:7: ( 'DECIMAL' )
            // PLSQL3.g:28:9: 'DECIMAL'
            {
            match("DECIMAL"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__71"

    // $ANTLR start "T__72"
    public final void mT__72() throws RecognitionException {
        try {
            int _type = T__72;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:29:7: ( 'DEC' )
            // PLSQL3.g:29:9: 'DEC'
            {
            match("DEC"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__72"

    // $ANTLR start "T__73"
    public final void mT__73() throws RecognitionException {
        try {
            int _type = T__73;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:30:7: ( 'LONG' )
            // PLSQL3.g:30:9: 'LONG'
            {
            match("LONG"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__73"

    // $ANTLR start "T__74"
    public final void mT__74() throws RecognitionException {
        try {
            int _type = T__74;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:31:7: ( 'RAW' )
            // PLSQL3.g:31:9: 'RAW'
            {
            match("RAW"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__74"

    // $ANTLR start "T__75"
    public final void mT__75() throws RecognitionException {
        try {
            int _type = T__75;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:32:7: ( 'BOOLEAN' )
            // PLSQL3.g:32:9: 'BOOLEAN'
            {
            match("BOOLEAN"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__75"

    // $ANTLR start "T__76"
    public final void mT__76() throws RecognitionException {
        try {
            int _type = T__76;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:33:7: ( 'DATE' )
            // PLSQL3.g:33:9: 'DATE'
            {
            match("DATE"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__76"

    // $ANTLR start "T__77"
    public final void mT__77() throws RecognitionException {
        try {
            int _type = T__77;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:34:7: ( 'TO' )
            // PLSQL3.g:34:9: 'TO'
            {
            match("TO"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__77"

    // $ANTLR start "T__78"
    public final void mT__78() throws RecognitionException {
        try {
            int _type = T__78;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:35:7: ( 'WITH' )
            // PLSQL3.g:35:9: 'WITH'
            {
            match("WITH"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__78"

    // $ANTLR start "T__79"
    public final void mT__79() throws RecognitionException {
        try {
            int _type = T__79;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:36:7: ( 'INTEGER' )
            // PLSQL3.g:36:9: 'INTEGER'
            {
            match("INTEGER"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__79"

    // $ANTLR start "T__80"
    public final void mT__80() throws RecognitionException {
        try {
            int _type = T__80;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:37:7: ( 'INT' )
            // PLSQL3.g:37:9: 'INT'
            {
            match("INT"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__80"

    // $ANTLR start "T__81"
    public final void mT__81() throws RecognitionException {
        try {
            int _type = T__81;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:38:7: ( 'SMALLINT' )
            // PLSQL3.g:38:9: 'SMALLINT'
            {
            match("SMALLINT"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__81"

    // $ANTLR start "T__82"
    public final void mT__82() throws RecognitionException {
        try {
            int _type = T__82;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:39:7: ( 'FLOAT' )
            // PLSQL3.g:39:9: 'FLOAT'
            {
            match("FLOAT"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__82"

    // $ANTLR start "T__83"
    public final void mT__83() throws RecognitionException {
        try {
            int _type = T__83;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:40:7: ( 'REAL' )
            // PLSQL3.g:40:9: 'REAL'
            {
            match("REAL"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__83"

    // $ANTLR start "T__84"
    public final void mT__84() throws RecognitionException {
        try {
            int _type = T__84;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:41:7: ( 'DOUBLE' )
            // PLSQL3.g:41:9: 'DOUBLE'
            {
            match("DOUBLE"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__84"

    // $ANTLR start "T__85"
    public final void mT__85() throws RecognitionException {
        try {
            int _type = T__85;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:42:7: ( 'CHAR' )
            // PLSQL3.g:42:9: 'CHAR'
            {
            match("CHAR"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__85"

    // $ANTLR start "T__86"
    public final void mT__86() throws RecognitionException {
        try {
            int _type = T__86;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:43:7: ( 'CHARACTER' )
            // PLSQL3.g:43:9: 'CHARACTER'
            {
            match("CHARACTER"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__86"

    // $ANTLR start "T__87"
    public final void mT__87() throws RecognitionException {
        try {
            int _type = T__87;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:44:7: ( 'SET' )
            // PLSQL3.g:44:9: 'SET'
            {
            match("SET"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__87"

    // $ANTLR start "T__88"
    public final void mT__88() throws RecognitionException {
        try {
            int _type = T__88;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:45:7: ( 'VARCHAR' )
            // PLSQL3.g:45:9: 'VARCHAR'
            {
            match("VARCHAR"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__88"

    // $ANTLR start "T__89"
    public final void mT__89() throws RecognitionException {
        try {
            int _type = T__89;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:46:7: ( 'VARCHAR2' )
            // PLSQL3.g:46:9: 'VARCHAR2'
            {
            match("VARCHAR2"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__89"

    // $ANTLR start "T__90"
    public final void mT__90() throws RecognitionException {
        try {
            int _type = T__90;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:47:7: ( 'NCHAR' )
            // PLSQL3.g:47:9: 'NCHAR'
            {
            match("NCHAR"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__90"

    // $ANTLR start "T__91"
    public final void mT__91() throws RecognitionException {
        try {
            int _type = T__91;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:48:7: ( 'NVARCHAR' )
            // PLSQL3.g:48:9: 'NVARCHAR'
            {
            match("NVARCHAR"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__91"

    // $ANTLR start "T__92"
    public final void mT__92() throws RecognitionException {
        try {
            int _type = T__92;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:49:7: ( 'NVARCHAR2' )
            // PLSQL3.g:49:9: 'NVARCHAR2'
            {
            match("NVARCHAR2"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__92"

    // $ANTLR start "T__93"
    public final void mT__93() throws RecognitionException {
        try {
            int _type = T__93;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:50:7: ( 'NATIONAL' )
            // PLSQL3.g:50:9: 'NATIONAL'
            {
            match("NATIONAL"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__93"

    // $ANTLR start "T__94"
    public final void mT__94() throws RecognitionException {
        try {
            int _type = T__94;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:51:7: ( 'MLSLABEL' )
            // PLSQL3.g:51:9: 'MLSLABEL'
            {
            match("MLSLABEL"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__94"

    // $ANTLR start "T__95"
    public final void mT__95() throws RecognitionException {
        try {
            int _type = T__95;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:52:7: ( 'PLS_INTEGER' )
            // PLSQL3.g:52:9: 'PLS_INTEGER'
            {
            match("PLS_INTEGER"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__95"

    // $ANTLR start "T__96"
    public final void mT__96() throws RecognitionException {
        try {
            int _type = T__96;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:53:7: ( 'BLOB' )
            // PLSQL3.g:53:9: 'BLOB'
            {
            match("BLOB"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__96"

    // $ANTLR start "T__97"
    public final void mT__97() throws RecognitionException {
        try {
            int _type = T__97;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:54:7: ( 'CLOB' )
            // PLSQL3.g:54:9: 'CLOB'
            {
            match("CLOB"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__97"

    // $ANTLR start "T__98"
    public final void mT__98() throws RecognitionException {
        try {
            int _type = T__98;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:55:7: ( 'NCLOB' )
            // PLSQL3.g:55:9: 'NCLOB'
            {
            match("NCLOB"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__98"

    // $ANTLR start "T__99"
    public final void mT__99() throws RecognitionException {
        try {
            int _type = T__99;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:56:7: ( 'BFILE' )
            // PLSQL3.g:56:9: 'BFILE'
            {
            match("BFILE"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__99"

    // $ANTLR start "T__100"
    public final void mT__100() throws RecognitionException {
        try {
            int _type = T__100;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:57:8: ( 'ROWID' )
            // PLSQL3.g:57:10: 'ROWID'
            {
            match("ROWID"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__100"

    // $ANTLR start "T__101"
    public final void mT__101() throws RecognitionException {
        try {
            int _type = T__101;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:58:8: ( 'UROWID' )
            // PLSQL3.g:58:10: 'UROWID'
            {
            match("UROWID"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__101"

    // $ANTLR start "T__102"
    public final void mT__102() throws RecognitionException {
        try {
            int _type = T__102;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:59:8: ( 'IN' )
            // PLSQL3.g:59:10: 'IN'
            {
            match("IN"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__102"

    // $ANTLR start "T__103"
    public final void mT__103() throws RecognitionException {
        try {
            int _type = T__103;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:60:8: ( 'PROCEDURE' )
            // PLSQL3.g:60:10: 'PROCEDURE'
            {
            match("PROCEDURE"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__103"

    // $ANTLR start "T__104"
    public final void mT__104() throws RecognitionException {
        try {
            int _type = T__104;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:61:8: ( 'FUNCTION' )
            // PLSQL3.g:61:10: 'FUNCTION'
            {
            match("FUNCTION"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__104"

    // $ANTLR start "T__105"
    public final void mT__105() throws RecognitionException {
        try {
            int _type = T__105;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:62:8: ( 'TABLE' )
            // PLSQL3.g:62:10: 'TABLE'
            {
            match("TABLE"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__105"

    // $ANTLR start "T__106"
    public final void mT__106() throws RecognitionException {
        try {
            int _type = T__106;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:63:8: ( 'OF' )
            // PLSQL3.g:63:10: 'OF'
            {
            match("OF"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__106"

    // $ANTLR start "T__107"
    public final void mT__107() throws RecognitionException {
        try {
            int _type = T__107;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:64:8: ( 'INDEX' )
            // PLSQL3.g:64:10: 'INDEX'
            {
            match("INDEX"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__107"

    // $ANTLR start "T__108"
    public final void mT__108() throws RecognitionException {
        try {
            int _type = T__108;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:65:8: ( 'BY' )
            // PLSQL3.g:65:10: 'BY'
            {
            match("BY"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__108"

    // $ANTLR start "T__109"
    public final void mT__109() throws RecognitionException {
        try {
            int _type = T__109;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:66:8: ( 'THEN' )
            // PLSQL3.g:66:10: 'THEN'
            {
            match("THEN"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__109"

    // $ANTLR start "T__110"
    public final void mT__110() throws RecognitionException {
        try {
            int _type = T__110;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:67:8: ( 'TRUE' )
            // PLSQL3.g:67:10: 'TRUE'
            {
            match("TRUE"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__110"

    // $ANTLR start "T__111"
    public final void mT__111() throws RecognitionException {
        try {
            int _type = T__111;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:68:8: ( 'FALSE' )
            // PLSQL3.g:68:10: 'FALSE'
            {
            match("FALSE"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__111"

    // $ANTLR start "T__112"
    public final void mT__112() throws RecognitionException {
        try {
            int _type = T__112;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:69:8: ( 'FOR' )
            // PLSQL3.g:69:10: 'FOR'
            {
            match("FOR"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__112"

    // $ANTLR start "T__113"
    public final void mT__113() throws RecognitionException {
        try {
            int _type = T__113;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:70:8: ( 'COMMIT' )
            // PLSQL3.g:70:10: 'COMMIT'
            {
            match("COMMIT"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__113"

    // $ANTLR start "T__114"
    public final void mT__114() throws RecognitionException {
        try {
            int _type = T__114;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:71:8: ( 'IF' )
            // PLSQL3.g:71:10: 'IF'
            {
            match("IF"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__114"

    // $ANTLR start "T__115"
    public final void mT__115() throws RecognitionException {
        try {
            int _type = T__115;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:72:8: ( 'ELSE' )
            // PLSQL3.g:72:10: 'ELSE'
            {
            match("ELSE"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__115"

    // $ANTLR start "T__116"
    public final void mT__116() throws RecognitionException {
        try {
            int _type = T__116;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:73:8: ( 'SELECT' )
            // PLSQL3.g:73:10: 'SELECT'
            {
            match("SELECT"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__116"

    // $ANTLR start "T__117"
    public final void mT__117() throws RecognitionException {
        try {
            int _type = T__117;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:74:8: ( 'DISTINCT' )
            // PLSQL3.g:74:10: 'DISTINCT'
            {
            match("DISTINCT"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__117"

    // $ANTLR start "T__118"
    public final void mT__118() throws RecognitionException {
        try {
            int _type = T__118;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:75:8: ( 'UNIQUE' )
            // PLSQL3.g:75:10: 'UNIQUE'
            {
            match("UNIQUE"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__118"

    // $ANTLR start "T__119"
    public final void mT__119() throws RecognitionException {
        try {
            int _type = T__119;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:76:8: ( 'ALL' )
            // PLSQL3.g:76:10: 'ALL'
            {
            match("ALL"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__119"

    // $ANTLR start "T__120"
    public final void mT__120() throws RecognitionException {
        try {
            int _type = T__120;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:77:8: ( 'INTO' )
            // PLSQL3.g:77:10: 'INTO'
            {
            match("INTO"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__120"

    // $ANTLR start "T__121"
    public final void mT__121() throws RecognitionException {
        try {
            int _type = T__121;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:78:8: ( 'FROM' )
            // PLSQL3.g:78:10: 'FROM'
            {
            match("FROM"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__121"

    // $ANTLR start "T__122"
    public final void mT__122() throws RecognitionException {
        try {
            int _type = T__122;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:79:8: ( 'HAVING' )
            // PLSQL3.g:79:10: 'HAVING'
            {
            match("HAVING"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__122"

    // $ANTLR start "T__123"
    public final void mT__123() throws RecognitionException {
        try {
            int _type = T__123;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:80:8: ( 'UNION' )
            // PLSQL3.g:80:10: 'UNION'
            {
            match("UNION"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__123"

    // $ANTLR start "T__124"
    public final void mT__124() throws RecognitionException {
        try {
            int _type = T__124;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:81:8: ( 'INTERSECT' )
            // PLSQL3.g:81:10: 'INTERSECT'
            {
            match("INTERSECT"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__124"

    // $ANTLR start "T__125"
    public final void mT__125() throws RecognitionException {
        try {
            int _type = T__125;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:82:8: ( 'MINUS' )
            // PLSQL3.g:82:10: 'MINUS'
            {
            match("MINUS"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__125"

    // $ANTLR start "T__126"
    public final void mT__126() throws RecognitionException {
        try {
            int _type = T__126;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:83:8: ( 'ON' )
            // PLSQL3.g:83:10: 'ON'
            {
            match("ON"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__126"

    // $ANTLR start "T__127"
    public final void mT__127() throws RecognitionException {
        try {
            int _type = T__127;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:84:8: ( 'WHERE' )
            // PLSQL3.g:84:10: 'WHERE'
            {
            match("WHERE"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__127"

    // $ANTLR start "T__128"
    public final void mT__128() throws RecognitionException {
        try {
            int _type = T__128;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:85:8: ( 'START' )
            // PLSQL3.g:85:10: 'START'
            {
            match("START"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__128"

    // $ANTLR start "T__129"
    public final void mT__129() throws RecognitionException {
        try {
            int _type = T__129;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:86:8: ( 'CONNECT' )
            // PLSQL3.g:86:10: 'CONNECT'
            {
            match("CONNECT"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__129"

    // $ANTLR start "T__130"
    public final void mT__130() throws RecognitionException {
        try {
            int _type = T__130;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:87:8: ( 'GROUP' )
            // PLSQL3.g:87:10: 'GROUP'
            {
            match("GROUP"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__130"

    // $ANTLR start "T__131"
    public final void mT__131() throws RecognitionException {
        try {
            int _type = T__131;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:88:8: ( 'ROWS' )
            // PLSQL3.g:88:10: 'ROWS'
            {
            match("ROWS"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__131"

    // $ANTLR start "T__132"
    public final void mT__132() throws RecognitionException {
        try {
            int _type = T__132;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:89:8: ( 'UPDATE' )
            // PLSQL3.g:89:10: 'UPDATE'
            {
            match("UPDATE"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__132"

    // $ANTLR start "T__133"
    public final void mT__133() throws RecognitionException {
        try {
            int _type = T__133;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:90:8: ( 'ORDER' )
            // PLSQL3.g:90:10: 'ORDER'
            {
            match("ORDER"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__133"

    // $ANTLR start "T__134"
    public final void mT__134() throws RecognitionException {
        try {
            int _type = T__134;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:91:8: ( 'LIKE' )
            // PLSQL3.g:91:10: 'LIKE'
            {
            match("LIKE"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__134"

    // $ANTLR start "T__135"
    public final void mT__135() throws RecognitionException {
        try {
            int _type = T__135;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:92:8: ( 'ASC' )
            // PLSQL3.g:92:10: 'ASC'
            {
            match("ASC"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__135"

    // $ANTLR start "T__136"
    public final void mT__136() throws RecognitionException {
        try {
            int _type = T__136;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:93:8: ( 'DESC' )
            // PLSQL3.g:93:10: 'DESC'
            {
            match("DESC"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__136"

    // $ANTLR start "T__137"
    public final void mT__137() throws RecognitionException {
        try {
            int _type = T__137;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:94:8: ( 'NOWAIT' )
            // PLSQL3.g:94:10: 'NOWAIT'
            {
            match("NOWAIT"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__137"

    // $ANTLR start "T__138"
    public final void mT__138() throws RecognitionException {
        try {
            int _type = T__138;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:95:8: ( 'AND' )
            // PLSQL3.g:95:10: 'AND'
            {
            match("AND"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__138"

    // $ANTLR start "T__139"
    public final void mT__139() throws RecognitionException {
        try {
            int _type = T__139;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:96:8: ( 'BETWEEN' )
            // PLSQL3.g:96:10: 'BETWEEN'
            {
            match("BETWEEN"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__139"

    // $ANTLR start "T__140"
    public final void mT__140() throws RecognitionException {
        try {
            int _type = T__140;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:97:8: ( 'SQL' )
            // PLSQL3.g:97:10: 'SQL'
            {
            match("SQL"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__140"

    // $ANTLR start "T__141"
    public final void mT__141() throws RecognitionException {
        try {
            int _type = T__141;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:98:8: ( 'PRIOR' )
            // PLSQL3.g:98:10: 'PRIOR'
            {
            match("PRIOR"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__141"

    // $ANTLR start "T__142"
    public final void mT__142() throws RecognitionException {
        try {
            int _type = T__142;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:99:8: ( 'CASE' )
            // PLSQL3.g:99:10: 'CASE'
            {
            match("CASE"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__142"

    // $ANTLR start "T__143"
    public final void mT__143() throws RecognitionException {
        try {
            int _type = T__143;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:100:8: ( 'AT' )
            // PLSQL3.g:100:10: 'AT'
            {
            match("AT"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__143"

    // $ANTLR start "T__144"
    public final void mT__144() throws RecognitionException {
        try {
            int _type = T__144;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:101:8: ( 'EXISTS' )
            // PLSQL3.g:101:10: 'EXISTS'
            {
            match("EXISTS"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__144"

    // $ANTLR start "T__145"
    public final void mT__145() throws RecognitionException {
        try {
            int _type = T__145;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:102:8: ( 'DELETE' )
            // PLSQL3.g:102:10: 'DELETE'
            {
            match("DELETE"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__145"

    // $ANTLR start "T__146"
    public final void mT__146() throws RecognitionException {
        try {
            int _type = T__146;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:103:8: ( 'ANY' )
            // PLSQL3.g:103:10: 'ANY'
            {
            match("ANY"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__146"

    // $ANTLR start "T__147"
    public final void mT__147() throws RecognitionException {
        try {
            int _type = T__147;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:104:8: ( 'INSERT' )
            // PLSQL3.g:104:10: 'INSERT'
            {
            match("INSERT"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__147"

    // $ANTLR start "T__148"
    public final void mT__148() throws RecognitionException {
        try {
            int _type = T__148;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:105:8: ( 'VALUES' )
            // PLSQL3.g:105:10: 'VALUES'
            {
            match("VALUES"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__148"

    // $ANTLR start "T__149"
    public final void mT__149() throws RecognitionException {
        try {
            int _type = T__149;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:106:8: ( 'FETCH' )
            // PLSQL3.g:106:10: 'FETCH'
            {
            match("FETCH"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__149"

    // $ANTLR start "T__150"
    public final void mT__150() throws RecognitionException {
        try {
            int _type = T__150;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:107:8: ( 'LOCK' )
            // PLSQL3.g:107:10: 'LOCK'
            {
            match("LOCK"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__150"

    // $ANTLR start "T__151"
    public final void mT__151() throws RecognitionException {
        try {
            int _type = T__151;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:108:8: ( 'MODE' )
            // PLSQL3.g:108:10: 'MODE'
            {
            match("MODE"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__151"

    // $ANTLR start "T__152"
    public final void mT__152() throws RecognitionException {
        try {
            int _type = T__152;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:109:8: ( 'ROW' )
            // PLSQL3.g:109:10: 'ROW'
            {
            match("ROW"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__152"

    // $ANTLR start "T__153"
    public final void mT__153() throws RecognitionException {
        try {
            int _type = T__153;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:110:8: ( 'SHARE' )
            // PLSQL3.g:110:10: 'SHARE'
            {
            match("SHARE"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__153"

    // $ANTLR start "T__154"
    public final void mT__154() throws RecognitionException {
        try {
            int _type = T__154;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:111:8: ( 'EXCLUSIVE' )
            // PLSQL3.g:111:10: 'EXCLUSIVE'
            {
            match("EXCLUSIVE"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__154"

    // $ANTLR start "T__155"
    public final void mT__155() throws RecognitionException {
        try {
            int _type = T__155;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:112:8: ( 'SAVEPOINT' )
            // PLSQL3.g:112:10: 'SAVEPOINT'
            {
            match("SAVEPOINT"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__155"

    // $ANTLR start "T__156"
    public final void mT__156() throws RecognitionException {
        try {
            int _type = T__156;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:113:8: ( 'COMMENT' )
            // PLSQL3.g:113:10: 'COMMENT'
            {
            match("COMMENT"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__156"

    // $ANTLR start "T__157"
    public final void mT__157() throws RecognitionException {
        try {
            int _type = T__157;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:114:8: ( 'ELSIF' )
            // PLSQL3.g:114:10: 'ELSIF'
            {
            match("ELSIF"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__157"

    // $ANTLR start "T__158"
    public final void mT__158() throws RecognitionException {
        try {
            int _type = T__158;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:115:8: ( 'LOOP' )
            // PLSQL3.g:115:10: 'LOOP'
            {
            match("LOOP"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__158"

    // $ANTLR start "T__159"
    public final void mT__159() throws RecognitionException {
        try {
            int _type = T__159;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:116:8: ( 'OUT' )
            // PLSQL3.g:116:10: 'OUT'
            {
            match("OUT"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__159"

    // $ANTLR start "T__160"
    public final void mT__160() throws RecognitionException {
        try {
            int _type = T__160;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:117:8: ( 'PACKAGE' )
            // PLSQL3.g:117:10: 'PACKAGE'
            {
            match("PACKAGE"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__160"

    // $ANTLR start "T__161"
    public final void mT__161() throws RecognitionException {
        try {
            int _type = T__161;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:118:8: ( 'PRAGMA' )
            // PLSQL3.g:118:10: 'PRAGMA'
            {
            match("PRAGMA"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__161"

    // $ANTLR start "T__162"
    public final void mT__162() throws RecognitionException {
        try {
            int _type = T__162;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:119:8: ( 'RAISE' )
            // PLSQL3.g:119:10: 'RAISE'
            {
            match("RAISE"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__162"

    // $ANTLR start "T__163"
    public final void mT__163() throws RecognitionException {
        try {
            int _type = T__163;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:120:8: ( 'RECORD' )
            // PLSQL3.g:120:10: 'RECORD'
            {
            match("RECORD"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__163"

    // $ANTLR start "T__164"
    public final void mT__164() throws RecognitionException {
        try {
            int _type = T__164;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:121:8: ( 'RETURN' )
            // PLSQL3.g:121:10: 'RETURN'
            {
            match("RETURN"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__164"

    // $ANTLR start "T__165"
    public final void mT__165() throws RecognitionException {
        try {
            int _type = T__165;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:122:8: ( 'RETURNING' )
            // PLSQL3.g:122:10: 'RETURNING'
            {
            match("RETURNING"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__165"

    // $ANTLR start "T__166"
    public final void mT__166() throws RecognitionException {
        try {
            int _type = T__166;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:123:8: ( 'ROLLBACK' )
            // PLSQL3.g:123:10: 'ROLLBACK'
            {
            match("ROLLBACK"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__166"

    // $ANTLR start "T__167"
    public final void mT__167() throws RecognitionException {
        try {
            int _type = T__167;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:124:8: ( 'WHILE' )
            // PLSQL3.g:124:10: 'WHILE'
            {
            match("WHILE"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "T__167"

    // $ANTLR start "QUOTED_STRING"
    public final void mQUOTED_STRING() throws RecognitionException {
        try {
            int _type = QUOTED_STRING;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1457:2: ( ( 'n' )? '\\'' ( '\\'\\'' | ~ ( '\\'' ) )* '\\'' )
            // PLSQL3.g:1457:4: ( 'n' )? '\\'' ( '\\'\\'' | ~ ( '\\'' ) )* '\\''
            {
            // PLSQL3.g:1457:4: ( 'n' )?
            int alt1=2;
            int LA1_0 = input.LA(1);

            if ( (LA1_0=='n') ) {
                alt1=1;
            }
            switch (alt1) {
                case 1 :
                    // PLSQL3.g:1457:6: 'n'
                    {
                    match('n'); if (state.failed) return ;

                    }
                    break;

            }

            match('\''); if (state.failed) return ;
            // PLSQL3.g:1457:18: ( '\\'\\'' | ~ ( '\\'' ) )*
            loop2:
            do {
                int alt2=3;
                int LA2_0 = input.LA(1);

                if ( (LA2_0=='\'') ) {
                    int LA2_1 = input.LA(2);

                    if ( (LA2_1=='\'') ) {
                        alt2=1;
                    }


                }
                else if ( ((LA2_0>='\u0000' && LA2_0<='&')||(LA2_0>='(' && LA2_0<='\uFFFF')) ) {
                    alt2=2;
                }


                switch (alt2) {
            	case 1 :
            	    // PLSQL3.g:1457:20: '\\'\\''
            	    {
            	    match("''"); if (state.failed) return ;


            	    }
            	    break;
            	case 2 :
            	    // PLSQL3.g:1457:29: ~ ( '\\'' )
            	    {
            	    if ( (input.LA(1)>='\u0000' && input.LA(1)<='&')||(input.LA(1)>='(' && input.LA(1)<='\uFFFF') ) {
            	        input.consume();
            	    state.failed=false;
            	    }
            	    else {
            	        if (state.backtracking>0) {state.failed=true; return ;}
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;

            	default :
            	    break loop2;
                }
            } while (true);

            match('\''); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "QUOTED_STRING"

    // $ANTLR start "ID"
    public final void mID() throws RecognitionException {
        try {
            int _type = ID;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1460:5: ( 'A' .. 'Z' ( 'A' .. 'Z' | '0' .. '9' | '_' | '$' | '#' )* | DOUBLEQUOTED_STRING )
            int alt4=2;
            int LA4_0 = input.LA(1);

            if ( ((LA4_0>='A' && LA4_0<='Z')) ) {
                alt4=1;
            }
            else if ( (LA4_0=='\"') ) {
                alt4=2;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return ;}
                NoViableAltException nvae =
                    new NoViableAltException("", 4, 0, input);

                throw nvae;
            }
            switch (alt4) {
                case 1 :
                    // PLSQL3.g:1460:7: 'A' .. 'Z' ( 'A' .. 'Z' | '0' .. '9' | '_' | '$' | '#' )*
                    {
                    matchRange('A','Z'); if (state.failed) return ;
                    // PLSQL3.g:1460:18: ( 'A' .. 'Z' | '0' .. '9' | '_' | '$' | '#' )*
                    loop3:
                    do {
                        int alt3=2;
                        int LA3_0 = input.LA(1);

                        if ( ((LA3_0>='#' && LA3_0<='$')||(LA3_0>='0' && LA3_0<='9')||(LA3_0>='A' && LA3_0<='Z')||LA3_0=='_') ) {
                            alt3=1;
                        }


                        switch (alt3) {
                    	case 1 :
                    	    // PLSQL3.g:
                    	    {
                    	    if ( (input.LA(1)>='#' && input.LA(1)<='$')||(input.LA(1)>='0' && input.LA(1)<='9')||(input.LA(1)>='A' && input.LA(1)<='Z')||input.LA(1)=='_' ) {
                    	        input.consume();
                    	    state.failed=false;
                    	    }
                    	    else {
                    	        if (state.backtracking>0) {state.failed=true; return ;}
                    	        MismatchedSetException mse = new MismatchedSetException(null,input);
                    	        recover(mse);
                    	        throw mse;}


                    	    }
                    	    break;

                    	default :
                    	    break loop3;
                        }
                    } while (true);


                    }
                    break;
                case 2 :
                    // PLSQL3.g:1461:7: DOUBLEQUOTED_STRING
                    {
                    mDOUBLEQUOTED_STRING(); if (state.failed) return ;

                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "ID"

    // $ANTLR start "SEMI"
    public final void mSEMI() throws RecognitionException {
        try {
            int _type = SEMI;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1464:2: ( ';' )
            // PLSQL3.g:1464:4: ';'
            {
            match(';'); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "SEMI"

    // $ANTLR start "COLON"
    public final void mCOLON() throws RecognitionException {
        try {
            int _type = COLON;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1467:2: ( ':' )
            // PLSQL3.g:1467:4: ':'
            {
            match(':'); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "COLON"

    // $ANTLR start "DOUBLEDOT"
    public final void mDOUBLEDOT() throws RecognitionException {
        try {
            int _type = DOUBLEDOT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1470:2: ( POINT POINT )
            // PLSQL3.g:1470:4: POINT POINT
            {
            mPOINT(); if (state.failed) return ;
            mPOINT(); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "DOUBLEDOT"

    // $ANTLR start "DOT"
    public final void mDOT() throws RecognitionException {
        try {
            int _type = DOT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1473:2: ( POINT )
            // PLSQL3.g:1473:4: POINT
            {
            mPOINT(); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "DOT"

    // $ANTLR start "POINT"
    public final void mPOINT() throws RecognitionException {
        try {
            // PLSQL3.g:1477:2: ( '.' )
            // PLSQL3.g:1477:4: '.'
            {
            match('.'); if (state.failed) return ;

            }

        }
        finally {
        }
    }
    // $ANTLR end "POINT"

    // $ANTLR start "COMMA"
    public final void mCOMMA() throws RecognitionException {
        try {
            int _type = COMMA;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1480:2: ( ',' )
            // PLSQL3.g:1480:4: ','
            {
            match(','); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "COMMA"

    // $ANTLR start "EXPONENT"
    public final void mEXPONENT() throws RecognitionException {
        try {
            int _type = EXPONENT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1483:2: ( '**' )
            // PLSQL3.g:1483:4: '**'
            {
            match("**"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "EXPONENT"

    // $ANTLR start "ASTERISK"
    public final void mASTERISK() throws RecognitionException {
        try {
            int _type = ASTERISK;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1486:2: ( '*' )
            // PLSQL3.g:1486:4: '*'
            {
            match('*'); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "ASTERISK"

    // $ANTLR start "AT_SIGN"
    public final void mAT_SIGN() throws RecognitionException {
        try {
            int _type = AT_SIGN;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1489:2: ( '@' )
            // PLSQL3.g:1489:4: '@'
            {
            match('@'); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "AT_SIGN"

    // $ANTLR start "RPAREN"
    public final void mRPAREN() throws RecognitionException {
        try {
            int _type = RPAREN;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1492:2: ( ')' )
            // PLSQL3.g:1492:4: ')'
            {
            match(')'); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "RPAREN"

    // $ANTLR start "LPAREN"
    public final void mLPAREN() throws RecognitionException {
        try {
            int _type = LPAREN;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1495:2: ( '(' )
            // PLSQL3.g:1495:4: '('
            {
            match('('); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "LPAREN"

    // $ANTLR start "RBRACK"
    public final void mRBRACK() throws RecognitionException {
        try {
            int _type = RBRACK;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1498:2: ( ']' )
            // PLSQL3.g:1498:4: ']'
            {
            match(']'); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "RBRACK"

    // $ANTLR start "LBRACK"
    public final void mLBRACK() throws RecognitionException {
        try {
            int _type = LBRACK;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1501:2: ( '[' )
            // PLSQL3.g:1501:4: '['
            {
            match('['); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "LBRACK"

    // $ANTLR start "PLUS"
    public final void mPLUS() throws RecognitionException {
        try {
            int _type = PLUS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1504:2: ( '+' )
            // PLSQL3.g:1504:4: '+'
            {
            match('+'); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "PLUS"

    // $ANTLR start "MINUS"
    public final void mMINUS() throws RecognitionException {
        try {
            int _type = MINUS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1507:2: ( '-' )
            // PLSQL3.g:1507:4: '-'
            {
            match('-'); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "MINUS"

    // $ANTLR start "DIVIDE"
    public final void mDIVIDE() throws RecognitionException {
        try {
            int _type = DIVIDE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1510:2: ( '/' )
            // PLSQL3.g:1510:4: '/'
            {
            match('/'); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "DIVIDE"

    // $ANTLR start "EQ"
    public final void mEQ() throws RecognitionException {
        try {
            int _type = EQ;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1513:2: ( '=' )
            // PLSQL3.g:1513:4: '='
            {
            match('='); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "EQ"

    // $ANTLR start "PERCENTAGE"
    public final void mPERCENTAGE() throws RecognitionException {
        try {
            int _type = PERCENTAGE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1516:2: ( '%' )
            // PLSQL3.g:1516:4: '%'
            {
            match('%'); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "PERCENTAGE"

    // $ANTLR start "LLABEL"
    public final void mLLABEL() throws RecognitionException {
        try {
            int _type = LLABEL;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1519:2: ( '<<' )
            // PLSQL3.g:1519:4: '<<'
            {
            match("<<"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "LLABEL"

    // $ANTLR start "RLABEL"
    public final void mRLABEL() throws RecognitionException {
        try {
            int _type = RLABEL;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1522:2: ( '>>' )
            // PLSQL3.g:1522:4: '>>'
            {
            match(">>"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "RLABEL"

    // $ANTLR start "ASSIGN"
    public final void mASSIGN() throws RecognitionException {
        try {
            int _type = ASSIGN;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1525:2: ( ':=' )
            // PLSQL3.g:1525:4: ':='
            {
            match(":="); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "ASSIGN"

    // $ANTLR start "ARROW"
    public final void mARROW() throws RecognitionException {
        try {
            int _type = ARROW;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1528:2: ( '=>' )
            // PLSQL3.g:1528:4: '=>'
            {
            match("=>"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "ARROW"

    // $ANTLR start "VERTBAR"
    public final void mVERTBAR() throws RecognitionException {
        try {
            int _type = VERTBAR;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1531:2: ( '|' )
            // PLSQL3.g:1531:4: '|'
            {
            match('|'); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "VERTBAR"

    // $ANTLR start "DOUBLEVERTBAR"
    public final void mDOUBLEVERTBAR() throws RecognitionException {
        try {
            int _type = DOUBLEVERTBAR;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1534:2: ( '||' )
            // PLSQL3.g:1534:4: '||'
            {
            match("||"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "DOUBLEVERTBAR"

    // $ANTLR start "NOT_EQ"
    public final void mNOT_EQ() throws RecognitionException {
        try {
            int _type = NOT_EQ;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1537:2: ( '<>' | '!=' | '^=' )
            int alt5=3;
            switch ( input.LA(1) ) {
            case '<':
                {
                alt5=1;
                }
                break;
            case '!':
                {
                alt5=2;
                }
                break;
            case '^':
                {
                alt5=3;
                }
                break;
            default:
                if (state.backtracking>0) {state.failed=true; return ;}
                NoViableAltException nvae =
                    new NoViableAltException("", 5, 0, input);

                throw nvae;
            }

            switch (alt5) {
                case 1 :
                    // PLSQL3.g:1537:4: '<>'
                    {
                    match("<>"); if (state.failed) return ;


                    }
                    break;
                case 2 :
                    // PLSQL3.g:1537:11: '!='
                    {
                    match("!="); if (state.failed) return ;


                    }
                    break;
                case 3 :
                    // PLSQL3.g:1537:18: '^='
                    {
                    match("^="); if (state.failed) return ;


                    }
                    break;

            }
            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "NOT_EQ"

    // $ANTLR start "LTH"
    public final void mLTH() throws RecognitionException {
        try {
            int _type = LTH;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1540:2: ( '<' )
            // PLSQL3.g:1540:4: '<'
            {
            match('<'); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "LTH"

    // $ANTLR start "LEQ"
    public final void mLEQ() throws RecognitionException {
        try {
            int _type = LEQ;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1543:2: ( '<=' )
            // PLSQL3.g:1543:4: '<='
            {
            match("<="); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "LEQ"

    // $ANTLR start "GTH"
    public final void mGTH() throws RecognitionException {
        try {
            int _type = GTH;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1546:2: ( '>' )
            // PLSQL3.g:1546:4: '>'
            {
            match('>'); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "GTH"

    // $ANTLR start "GEQ"
    public final void mGEQ() throws RecognitionException {
        try {
            int _type = GEQ;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1549:2: ( '>=' )
            // PLSQL3.g:1549:4: '>='
            {
            match(">="); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "GEQ"

    // $ANTLR start "NUMBER"
    public final void mNUMBER() throws RecognitionException {
        try {
            int _type = NUMBER;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1552:2: ( ( ( N POINT N )=> N POINT N | POINT N | N ) ( 'E' ( PLUS | MINUS )? N )? )
            // PLSQL3.g:1553:3: ( ( N POINT N )=> N POINT N | POINT N | N ) ( 'E' ( PLUS | MINUS )? N )?
            {
            // PLSQL3.g:1553:3: ( ( N POINT N )=> N POINT N | POINT N | N )
            int alt6=3;
            alt6 = dfa6.predict(input);
            switch (alt6) {
                case 1 :
                    // PLSQL3.g:1553:5: ( N POINT N )=> N POINT N
                    {
                    mN(); if (state.failed) return ;
                    mPOINT(); if (state.failed) return ;
                    mN(); if (state.failed) return ;

                    }
                    break;
                case 2 :
                    // PLSQL3.g:1554:5: POINT N
                    {
                    mPOINT(); if (state.failed) return ;
                    mN(); if (state.failed) return ;

                    }
                    break;
                case 3 :
                    // PLSQL3.g:1555:5: N
                    {
                    mN(); if (state.failed) return ;

                    }
                    break;

            }

            // PLSQL3.g:1557:3: ( 'E' ( PLUS | MINUS )? N )?
            int alt8=2;
            int LA8_0 = input.LA(1);

            if ( (LA8_0=='E') ) {
                alt8=1;
            }
            switch (alt8) {
                case 1 :
                    // PLSQL3.g:1557:5: 'E' ( PLUS | MINUS )? N
                    {
                    match('E'); if (state.failed) return ;
                    // PLSQL3.g:1557:9: ( PLUS | MINUS )?
                    int alt7=2;
                    int LA7_0 = input.LA(1);

                    if ( (LA7_0=='+'||LA7_0=='-') ) {
                        alt7=1;
                    }
                    switch (alt7) {
                        case 1 :
                            // PLSQL3.g:
                            {
                            if ( input.LA(1)=='+'||input.LA(1)=='-' ) {
                                input.consume();
                            state.failed=false;
                            }
                            else {
                                if (state.backtracking>0) {state.failed=true; return ;}
                                MismatchedSetException mse = new MismatchedSetException(null,input);
                                recover(mse);
                                throw mse;}


                            }
                            break;

                    }

                    mN(); if (state.failed) return ;

                    }
                    break;

            }


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "NUMBER"

    // $ANTLR start "N"
    public final void mN() throws RecognitionException {
        try {
            // PLSQL3.g:1561:2: ( '0' .. '9' ( '0' .. '9' )* )
            // PLSQL3.g:1561:4: '0' .. '9' ( '0' .. '9' )*
            {
            matchRange('0','9'); if (state.failed) return ;
            // PLSQL3.g:1561:15: ( '0' .. '9' )*
            loop9:
            do {
                int alt9=2;
                int LA9_0 = input.LA(1);

                if ( ((LA9_0>='0' && LA9_0<='9')) ) {
                    alt9=1;
                }


                switch (alt9) {
            	case 1 :
            	    // PLSQL3.g:1561:17: '0' .. '9'
            	    {
            	    matchRange('0','9'); if (state.failed) return ;

            	    }
            	    break;

            	default :
            	    break loop9;
                }
            } while (true);


            }

        }
        finally {
        }
    }
    // $ANTLR end "N"

    // $ANTLR start "QUOTE"
    public final void mQUOTE() throws RecognitionException {
        try {
            int _type = QUOTE;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1564:2: ( '\\'' )
            // PLSQL3.g:1564:4: '\\''
            {
            match('\''); if (state.failed) return ;

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "QUOTE"

    // $ANTLR start "DOUBLEQUOTED_STRING"
    public final void mDOUBLEQUOTED_STRING() throws RecognitionException {
        try {
            // PLSQL3.g:1568:2: ( '\"' (~ ( '\"' ) )* '\"' )
            // PLSQL3.g:1568:4: '\"' (~ ( '\"' ) )* '\"'
            {
            match('\"'); if (state.failed) return ;
            // PLSQL3.g:1568:8: (~ ( '\"' ) )*
            loop10:
            do {
                int alt10=2;
                int LA10_0 = input.LA(1);

                if ( ((LA10_0>='\u0000' && LA10_0<='!')||(LA10_0>='#' && LA10_0<='\uFFFF')) ) {
                    alt10=1;
                }


                switch (alt10) {
            	case 1 :
            	    // PLSQL3.g:1568:10: ~ ( '\"' )
            	    {
            	    if ( (input.LA(1)>='\u0000' && input.LA(1)<='!')||(input.LA(1)>='#' && input.LA(1)<='\uFFFF') ) {
            	        input.consume();
            	    state.failed=false;
            	    }
            	    else {
            	        if (state.backtracking>0) {state.failed=true; return ;}
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;

            	default :
            	    break loop10;
                }
            } while (true);

            match('\"'); if (state.failed) return ;

            }

        }
        finally {
        }
    }
    // $ANTLR end "DOUBLEQUOTED_STRING"

    // $ANTLR start "WS"
    public final void mWS() throws RecognitionException {
        try {
            int _type = WS;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1570:4: ( ( ' ' | '\\r' | '\\t' | '\\n' ) )
            // PLSQL3.g:1570:6: ( ' ' | '\\r' | '\\t' | '\\n' )
            {
            if ( (input.LA(1)>='\t' && input.LA(1)<='\n')||input.LA(1)=='\r'||input.LA(1)==' ' ) {
                input.consume();
            state.failed=false;
            }
            else {
                if (state.backtracking>0) {state.failed=true; return ;}
                MismatchedSetException mse = new MismatchedSetException(null,input);
                recover(mse);
                throw mse;}

            if ( state.backtracking==0 ) {
              _channel=HIDDEN;
            }

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "WS"

    // $ANTLR start "SL_COMMENT"
    public final void mSL_COMMENT() throws RecognitionException {
        try {
            int _type = SL_COMMENT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1573:2: ( '--' (~ ( '\\n' | '\\r' ) )* ( '\\r' )? '\\n' )
            // PLSQL3.g:1573:4: '--' (~ ( '\\n' | '\\r' ) )* ( '\\r' )? '\\n'
            {
            match("--"); if (state.failed) return ;

            // PLSQL3.g:1573:9: (~ ( '\\n' | '\\r' ) )*
            loop11:
            do {
                int alt11=2;
                int LA11_0 = input.LA(1);

                if ( ((LA11_0>='\u0000' && LA11_0<='\t')||(LA11_0>='\u000B' && LA11_0<='\f')||(LA11_0>='\u000E' && LA11_0<='\uFFFF')) ) {
                    alt11=1;
                }


                switch (alt11) {
            	case 1 :
            	    // PLSQL3.g:1573:9: ~ ( '\\n' | '\\r' )
            	    {
            	    if ( (input.LA(1)>='\u0000' && input.LA(1)<='\t')||(input.LA(1)>='\u000B' && input.LA(1)<='\f')||(input.LA(1)>='\u000E' && input.LA(1)<='\uFFFF') ) {
            	        input.consume();
            	    state.failed=false;
            	    }
            	    else {
            	        if (state.backtracking>0) {state.failed=true; return ;}
            	        MismatchedSetException mse = new MismatchedSetException(null,input);
            	        recover(mse);
            	        throw mse;}


            	    }
            	    break;

            	default :
            	    break loop11;
                }
            } while (true);

            // PLSQL3.g:1573:23: ( '\\r' )?
            int alt12=2;
            int LA12_0 = input.LA(1);

            if ( (LA12_0=='\r') ) {
                alt12=1;
            }
            switch (alt12) {
                case 1 :
                    // PLSQL3.g:1573:23: '\\r'
                    {
                    match('\r'); if (state.failed) return ;

                    }
                    break;

            }

            match('\n'); if (state.failed) return ;
            if ( state.backtracking==0 ) {
              _channel=HIDDEN;
            }

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "SL_COMMENT"

    // $ANTLR start "ML_COMMENT"
    public final void mML_COMMENT() throws RecognitionException {
        try {
            int _type = ML_COMMENT;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1576:2: ( '/*' ( options {greedy=false; } : . )* '*/' )
            // PLSQL3.g:1576:4: '/*' ( options {greedy=false; } : . )* '*/'
            {
            match("/*"); if (state.failed) return ;

            // PLSQL3.g:1576:9: ( options {greedy=false; } : . )*
            loop13:
            do {
                int alt13=2;
                int LA13_0 = input.LA(1);

                if ( (LA13_0=='*') ) {
                    int LA13_1 = input.LA(2);

                    if ( (LA13_1=='/') ) {
                        alt13=2;
                    }
                    else if ( ((LA13_1>='\u0000' && LA13_1<='.')||(LA13_1>='0' && LA13_1<='\uFFFF')) ) {
                        alt13=1;
                    }


                }
                else if ( ((LA13_0>='\u0000' && LA13_0<=')')||(LA13_0>='+' && LA13_0<='\uFFFF')) ) {
                    alt13=1;
                }


                switch (alt13) {
            	case 1 :
            	    // PLSQL3.g:1576:37: .
            	    {
            	    matchAny(); if (state.failed) return ;

            	    }
            	    break;

            	default :
            	    break loop13;
                }
            } while (true);

            match("*/"); if (state.failed) return ;

            if ( state.backtracking==0 ) {
              _channel=HIDDEN;
            }

            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "ML_COMMENT"

    // $ANTLR start "TYPE_ATTR"
    public final void mTYPE_ATTR() throws RecognitionException {
        try {
            int _type = TYPE_ATTR;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1579:2: ( '%TYPE' )
            // PLSQL3.g:1579:4: '%TYPE'
            {
            match("%TYPE"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "TYPE_ATTR"

    // $ANTLR start "ROWTYPE_ATTR"
    public final void mROWTYPE_ATTR() throws RecognitionException {
        try {
            int _type = ROWTYPE_ATTR;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1582:2: ( '%ROWTYPE' )
            // PLSQL3.g:1582:4: '%ROWTYPE'
            {
            match("%ROWTYPE"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "ROWTYPE_ATTR"

    // $ANTLR start "NOTFOUND_ATTR"
    public final void mNOTFOUND_ATTR() throws RecognitionException {
        try {
            int _type = NOTFOUND_ATTR;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1585:2: ( '%NOTFOUND' )
            // PLSQL3.g:1585:4: '%NOTFOUND'
            {
            match("%NOTFOUND"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "NOTFOUND_ATTR"

    // $ANTLR start "FOUND_ATTR"
    public final void mFOUND_ATTR() throws RecognitionException {
        try {
            int _type = FOUND_ATTR;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1588:2: ( '%FOUND' )
            // PLSQL3.g:1588:4: '%FOUND'
            {
            match("%FOUND"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "FOUND_ATTR"

    // $ANTLR start "ISOPEN_ATTR"
    public final void mISOPEN_ATTR() throws RecognitionException {
        try {
            int _type = ISOPEN_ATTR;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1591:2: ( '%ISOPEN' )
            // PLSQL3.g:1591:4: '%ISOPEN'
            {
            match("%ISOPEN"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "ISOPEN_ATTR"

    // $ANTLR start "ROWCOUNT_ATTR"
    public final void mROWCOUNT_ATTR() throws RecognitionException {
        try {
            int _type = ROWCOUNT_ATTR;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1594:2: ( '%ROWCOUNT' )
            // PLSQL3.g:1594:4: '%ROWCOUNT'
            {
            match("%ROWCOUNT"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "ROWCOUNT_ATTR"

    // $ANTLR start "BULK_ROWCOUNT_ATTR"
    public final void mBULK_ROWCOUNT_ATTR() throws RecognitionException {
        try {
            int _type = BULK_ROWCOUNT_ATTR;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1597:2: ( '%BULK_ROWCOUNT' )
            // PLSQL3.g:1597:4: '%BULK_ROWCOUNT'
            {
            match("%BULK_ROWCOUNT"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "BULK_ROWCOUNT_ATTR"

    // $ANTLR start "CHARSET_ATTR"
    public final void mCHARSET_ATTR() throws RecognitionException {
        try {
            int _type = CHARSET_ATTR;
            int _channel = DEFAULT_TOKEN_CHANNEL;
            // PLSQL3.g:1600:2: ( '%CHARSET' )
            // PLSQL3.g:1600:4: '%CHARSET'
            {
            match("%CHARSET"); if (state.failed) return ;


            }

            state.type = _type;
            state.channel = _channel;
        }
        finally {
        }
    }
    // $ANTLR end "CHARSET_ATTR"

    public void mTokens() throws RecognitionException {
        // PLSQL3.g:1:8: ( T__50 | T__51 | T__52 | T__53 | T__54 | T__55 | T__56 | T__57 | T__58 | T__59 | T__60 | T__61 | T__62 | T__63 | T__64 | T__65 | T__66 | T__67 | T__68 | T__69 | T__70 | T__71 | T__72 | T__73 | T__74 | T__75 | T__76 | T__77 | T__78 | T__79 | T__80 | T__81 | T__82 | T__83 | T__84 | T__85 | T__86 | T__87 | T__88 | T__89 | T__90 | T__91 | T__92 | T__93 | T__94 | T__95 | T__96 | T__97 | T__98 | T__99 | T__100 | T__101 | T__102 | T__103 | T__104 | T__105 | T__106 | T__107 | T__108 | T__109 | T__110 | T__111 | T__112 | T__113 | T__114 | T__115 | T__116 | T__117 | T__118 | T__119 | T__120 | T__121 | T__122 | T__123 | T__124 | T__125 | T__126 | T__127 | T__128 | T__129 | T__130 | T__131 | T__132 | T__133 | T__134 | T__135 | T__136 | T__137 | T__138 | T__139 | T__140 | T__141 | T__142 | T__143 | T__144 | T__145 | T__146 | T__147 | T__148 | T__149 | T__150 | T__151 | T__152 | T__153 | T__154 | T__155 | T__156 | T__157 | T__158 | T__159 | T__160 | T__161 | T__162 | T__163 | T__164 | T__165 | T__166 | T__167 | QUOTED_STRING | ID | SEMI | COLON | DOUBLEDOT | DOT | COMMA | EXPONENT | ASTERISK | AT_SIGN | RPAREN | LPAREN | RBRACK | LBRACK | PLUS | MINUS | DIVIDE | EQ | PERCENTAGE | LLABEL | RLABEL | ASSIGN | ARROW | VERTBAR | DOUBLEVERTBAR | NOT_EQ | LTH | LEQ | GTH | GEQ | NUMBER | QUOTE | WS | SL_COMMENT | ML_COMMENT | TYPE_ATTR | ROWTYPE_ATTR | NOTFOUND_ATTR | FOUND_ATTR | ISOPEN_ATTR | ROWCOUNT_ATTR | BULK_ROWCOUNT_ATTR | CHARSET_ATTR )
        int alt14=161;
        alt14 = dfa14.predict(input);
        switch (alt14) {
            case 1 :
                // PLSQL3.g:1:10: T__50
                {
                mT__50(); if (state.failed) return ;

                }
                break;
            case 2 :
                // PLSQL3.g:1:16: T__51
                {
                mT__51(); if (state.failed) return ;

                }
                break;
            case 3 :
                // PLSQL3.g:1:22: T__52
                {
                mT__52(); if (state.failed) return ;

                }
                break;
            case 4 :
                // PLSQL3.g:1:28: T__53
                {
                mT__53(); if (state.failed) return ;

                }
                break;
            case 5 :
                // PLSQL3.g:1:34: T__54
                {
                mT__54(); if (state.failed) return ;

                }
                break;
            case 6 :
                // PLSQL3.g:1:40: T__55
                {
                mT__55(); if (state.failed) return ;

                }
                break;
            case 7 :
                // PLSQL3.g:1:46: T__56
                {
                mT__56(); if (state.failed) return ;

                }
                break;
            case 8 :
                // PLSQL3.g:1:52: T__57
                {
                mT__57(); if (state.failed) return ;

                }
                break;
            case 9 :
                // PLSQL3.g:1:58: T__58
                {
                mT__58(); if (state.failed) return ;

                }
                break;
            case 10 :
                // PLSQL3.g:1:64: T__59
                {
                mT__59(); if (state.failed) return ;

                }
                break;
            case 11 :
                // PLSQL3.g:1:70: T__60
                {
                mT__60(); if (state.failed) return ;

                }
                break;
            case 12 :
                // PLSQL3.g:1:76: T__61
                {
                mT__61(); if (state.failed) return ;

                }
                break;
            case 13 :
                // PLSQL3.g:1:82: T__62
                {
                mT__62(); if (state.failed) return ;

                }
                break;
            case 14 :
                // PLSQL3.g:1:88: T__63
                {
                mT__63(); if (state.failed) return ;

                }
                break;
            case 15 :
                // PLSQL3.g:1:94: T__64
                {
                mT__64(); if (state.failed) return ;

                }
                break;
            case 16 :
                // PLSQL3.g:1:100: T__65
                {
                mT__65(); if (state.failed) return ;

                }
                break;
            case 17 :
                // PLSQL3.g:1:106: T__66
                {
                mT__66(); if (state.failed) return ;

                }
                break;
            case 18 :
                // PLSQL3.g:1:112: T__67
                {
                mT__67(); if (state.failed) return ;

                }
                break;
            case 19 :
                // PLSQL3.g:1:118: T__68
                {
                mT__68(); if (state.failed) return ;

                }
                break;
            case 20 :
                // PLSQL3.g:1:124: T__69
                {
                mT__69(); if (state.failed) return ;

                }
                break;
            case 21 :
                // PLSQL3.g:1:130: T__70
                {
                mT__70(); if (state.failed) return ;

                }
                break;
            case 22 :
                // PLSQL3.g:1:136: T__71
                {
                mT__71(); if (state.failed) return ;

                }
                break;
            case 23 :
                // PLSQL3.g:1:142: T__72
                {
                mT__72(); if (state.failed) return ;

                }
                break;
            case 24 :
                // PLSQL3.g:1:148: T__73
                {
                mT__73(); if (state.failed) return ;

                }
                break;
            case 25 :
                // PLSQL3.g:1:154: T__74
                {
                mT__74(); if (state.failed) return ;

                }
                break;
            case 26 :
                // PLSQL3.g:1:160: T__75
                {
                mT__75(); if (state.failed) return ;

                }
                break;
            case 27 :
                // PLSQL3.g:1:166: T__76
                {
                mT__76(); if (state.failed) return ;

                }
                break;
            case 28 :
                // PLSQL3.g:1:172: T__77
                {
                mT__77(); if (state.failed) return ;

                }
                break;
            case 29 :
                // PLSQL3.g:1:178: T__78
                {
                mT__78(); if (state.failed) return ;

                }
                break;
            case 30 :
                // PLSQL3.g:1:184: T__79
                {
                mT__79(); if (state.failed) return ;

                }
                break;
            case 31 :
                // PLSQL3.g:1:190: T__80
                {
                mT__80(); if (state.failed) return ;

                }
                break;
            case 32 :
                // PLSQL3.g:1:196: T__81
                {
                mT__81(); if (state.failed) return ;

                }
                break;
            case 33 :
                // PLSQL3.g:1:202: T__82
                {
                mT__82(); if (state.failed) return ;

                }
                break;
            case 34 :
                // PLSQL3.g:1:208: T__83
                {
                mT__83(); if (state.failed) return ;

                }
                break;
            case 35 :
                // PLSQL3.g:1:214: T__84
                {
                mT__84(); if (state.failed) return ;

                }
                break;
            case 36 :
                // PLSQL3.g:1:220: T__85
                {
                mT__85(); if (state.failed) return ;

                }
                break;
            case 37 :
                // PLSQL3.g:1:226: T__86
                {
                mT__86(); if (state.failed) return ;

                }
                break;
            case 38 :
                // PLSQL3.g:1:232: T__87
                {
                mT__87(); if (state.failed) return ;

                }
                break;
            case 39 :
                // PLSQL3.g:1:238: T__88
                {
                mT__88(); if (state.failed) return ;

                }
                break;
            case 40 :
                // PLSQL3.g:1:244: T__89
                {
                mT__89(); if (state.failed) return ;

                }
                break;
            case 41 :
                // PLSQL3.g:1:250: T__90
                {
                mT__90(); if (state.failed) return ;

                }
                break;
            case 42 :
                // PLSQL3.g:1:256: T__91
                {
                mT__91(); if (state.failed) return ;

                }
                break;
            case 43 :
                // PLSQL3.g:1:262: T__92
                {
                mT__92(); if (state.failed) return ;

                }
                break;
            case 44 :
                // PLSQL3.g:1:268: T__93
                {
                mT__93(); if (state.failed) return ;

                }
                break;
            case 45 :
                // PLSQL3.g:1:274: T__94
                {
                mT__94(); if (state.failed) return ;

                }
                break;
            case 46 :
                // PLSQL3.g:1:280: T__95
                {
                mT__95(); if (state.failed) return ;

                }
                break;
            case 47 :
                // PLSQL3.g:1:286: T__96
                {
                mT__96(); if (state.failed) return ;

                }
                break;
            case 48 :
                // PLSQL3.g:1:292: T__97
                {
                mT__97(); if (state.failed) return ;

                }
                break;
            case 49 :
                // PLSQL3.g:1:298: T__98
                {
                mT__98(); if (state.failed) return ;

                }
                break;
            case 50 :
                // PLSQL3.g:1:304: T__99
                {
                mT__99(); if (state.failed) return ;

                }
                break;
            case 51 :
                // PLSQL3.g:1:310: T__100
                {
                mT__100(); if (state.failed) return ;

                }
                break;
            case 52 :
                // PLSQL3.g:1:317: T__101
                {
                mT__101(); if (state.failed) return ;

                }
                break;
            case 53 :
                // PLSQL3.g:1:324: T__102
                {
                mT__102(); if (state.failed) return ;

                }
                break;
            case 54 :
                // PLSQL3.g:1:331: T__103
                {
                mT__103(); if (state.failed) return ;

                }
                break;
            case 55 :
                // PLSQL3.g:1:338: T__104
                {
                mT__104(); if (state.failed) return ;

                }
                break;
            case 56 :
                // PLSQL3.g:1:345: T__105
                {
                mT__105(); if (state.failed) return ;

                }
                break;
            case 57 :
                // PLSQL3.g:1:352: T__106
                {
                mT__106(); if (state.failed) return ;

                }
                break;
            case 58 :
                // PLSQL3.g:1:359: T__107
                {
                mT__107(); if (state.failed) return ;

                }
                break;
            case 59 :
                // PLSQL3.g:1:366: T__108
                {
                mT__108(); if (state.failed) return ;

                }
                break;
            case 60 :
                // PLSQL3.g:1:373: T__109
                {
                mT__109(); if (state.failed) return ;

                }
                break;
            case 61 :
                // PLSQL3.g:1:380: T__110
                {
                mT__110(); if (state.failed) return ;

                }
                break;
            case 62 :
                // PLSQL3.g:1:387: T__111
                {
                mT__111(); if (state.failed) return ;

                }
                break;
            case 63 :
                // PLSQL3.g:1:394: T__112
                {
                mT__112(); if (state.failed) return ;

                }
                break;
            case 64 :
                // PLSQL3.g:1:401: T__113
                {
                mT__113(); if (state.failed) return ;

                }
                break;
            case 65 :
                // PLSQL3.g:1:408: T__114
                {
                mT__114(); if (state.failed) return ;

                }
                break;
            case 66 :
                // PLSQL3.g:1:415: T__115
                {
                mT__115(); if (state.failed) return ;

                }
                break;
            case 67 :
                // PLSQL3.g:1:422: T__116
                {
                mT__116(); if (state.failed) return ;

                }
                break;
            case 68 :
                // PLSQL3.g:1:429: T__117
                {
                mT__117(); if (state.failed) return ;

                }
                break;
            case 69 :
                // PLSQL3.g:1:436: T__118
                {
                mT__118(); if (state.failed) return ;

                }
                break;
            case 70 :
                // PLSQL3.g:1:443: T__119
                {
                mT__119(); if (state.failed) return ;

                }
                break;
            case 71 :
                // PLSQL3.g:1:450: T__120
                {
                mT__120(); if (state.failed) return ;

                }
                break;
            case 72 :
                // PLSQL3.g:1:457: T__121
                {
                mT__121(); if (state.failed) return ;

                }
                break;
            case 73 :
                // PLSQL3.g:1:464: T__122
                {
                mT__122(); if (state.failed) return ;

                }
                break;
            case 74 :
                // PLSQL3.g:1:471: T__123
                {
                mT__123(); if (state.failed) return ;

                }
                break;
            case 75 :
                // PLSQL3.g:1:478: T__124
                {
                mT__124(); if (state.failed) return ;

                }
                break;
            case 76 :
                // PLSQL3.g:1:485: T__125
                {
                mT__125(); if (state.failed) return ;

                }
                break;
            case 77 :
                // PLSQL3.g:1:492: T__126
                {
                mT__126(); if (state.failed) return ;

                }
                break;
            case 78 :
                // PLSQL3.g:1:499: T__127
                {
                mT__127(); if (state.failed) return ;

                }
                break;
            case 79 :
                // PLSQL3.g:1:506: T__128
                {
                mT__128(); if (state.failed) return ;

                }
                break;
            case 80 :
                // PLSQL3.g:1:513: T__129
                {
                mT__129(); if (state.failed) return ;

                }
                break;
            case 81 :
                // PLSQL3.g:1:520: T__130
                {
                mT__130(); if (state.failed) return ;

                }
                break;
            case 82 :
                // PLSQL3.g:1:527: T__131
                {
                mT__131(); if (state.failed) return ;

                }
                break;
            case 83 :
                // PLSQL3.g:1:534: T__132
                {
                mT__132(); if (state.failed) return ;

                }
                break;
            case 84 :
                // PLSQL3.g:1:541: T__133
                {
                mT__133(); if (state.failed) return ;

                }
                break;
            case 85 :
                // PLSQL3.g:1:548: T__134
                {
                mT__134(); if (state.failed) return ;

                }
                break;
            case 86 :
                // PLSQL3.g:1:555: T__135
                {
                mT__135(); if (state.failed) return ;

                }
                break;
            case 87 :
                // PLSQL3.g:1:562: T__136
                {
                mT__136(); if (state.failed) return ;

                }
                break;
            case 88 :
                // PLSQL3.g:1:569: T__137
                {
                mT__137(); if (state.failed) return ;

                }
                break;
            case 89 :
                // PLSQL3.g:1:576: T__138
                {
                mT__138(); if (state.failed) return ;

                }
                break;
            case 90 :
                // PLSQL3.g:1:583: T__139
                {
                mT__139(); if (state.failed) return ;

                }
                break;
            case 91 :
                // PLSQL3.g:1:590: T__140
                {
                mT__140(); if (state.failed) return ;

                }
                break;
            case 92 :
                // PLSQL3.g:1:597: T__141
                {
                mT__141(); if (state.failed) return ;

                }
                break;
            case 93 :
                // PLSQL3.g:1:604: T__142
                {
                mT__142(); if (state.failed) return ;

                }
                break;
            case 94 :
                // PLSQL3.g:1:611: T__143
                {
                mT__143(); if (state.failed) return ;

                }
                break;
            case 95 :
                // PLSQL3.g:1:618: T__144
                {
                mT__144(); if (state.failed) return ;

                }
                break;
            case 96 :
                // PLSQL3.g:1:625: T__145
                {
                mT__145(); if (state.failed) return ;

                }
                break;
            case 97 :
                // PLSQL3.g:1:632: T__146
                {
                mT__146(); if (state.failed) return ;

                }
                break;
            case 98 :
                // PLSQL3.g:1:639: T__147
                {
                mT__147(); if (state.failed) return ;

                }
                break;
            case 99 :
                // PLSQL3.g:1:646: T__148
                {
                mT__148(); if (state.failed) return ;

                }
                break;
            case 100 :
                // PLSQL3.g:1:653: T__149
                {
                mT__149(); if (state.failed) return ;

                }
                break;
            case 101 :
                // PLSQL3.g:1:660: T__150
                {
                mT__150(); if (state.failed) return ;

                }
                break;
            case 102 :
                // PLSQL3.g:1:667: T__151
                {
                mT__151(); if (state.failed) return ;

                }
                break;
            case 103 :
                // PLSQL3.g:1:674: T__152
                {
                mT__152(); if (state.failed) return ;

                }
                break;
            case 104 :
                // PLSQL3.g:1:681: T__153
                {
                mT__153(); if (state.failed) return ;

                }
                break;
            case 105 :
                // PLSQL3.g:1:688: T__154
                {
                mT__154(); if (state.failed) return ;

                }
                break;
            case 106 :
                // PLSQL3.g:1:695: T__155
                {
                mT__155(); if (state.failed) return ;

                }
                break;
            case 107 :
                // PLSQL3.g:1:702: T__156
                {
                mT__156(); if (state.failed) return ;

                }
                break;
            case 108 :
                // PLSQL3.g:1:709: T__157
                {
                mT__157(); if (state.failed) return ;

                }
                break;
            case 109 :
                // PLSQL3.g:1:716: T__158
                {
                mT__158(); if (state.failed) return ;

                }
                break;
            case 110 :
                // PLSQL3.g:1:723: T__159
                {
                mT__159(); if (state.failed) return ;

                }
                break;
            case 111 :
                // PLSQL3.g:1:730: T__160
                {
                mT__160(); if (state.failed) return ;

                }
                break;
            case 112 :
                // PLSQL3.g:1:737: T__161
                {
                mT__161(); if (state.failed) return ;

                }
                break;
            case 113 :
                // PLSQL3.g:1:744: T__162
                {
                mT__162(); if (state.failed) return ;

                }
                break;
            case 114 :
                // PLSQL3.g:1:751: T__163
                {
                mT__163(); if (state.failed) return ;

                }
                break;
            case 115 :
                // PLSQL3.g:1:758: T__164
                {
                mT__164(); if (state.failed) return ;

                }
                break;
            case 116 :
                // PLSQL3.g:1:765: T__165
                {
                mT__165(); if (state.failed) return ;

                }
                break;
            case 117 :
                // PLSQL3.g:1:772: T__166
                {
                mT__166(); if (state.failed) return ;

                }
                break;
            case 118 :
                // PLSQL3.g:1:779: T__167
                {
                mT__167(); if (state.failed) return ;

                }
                break;
            case 119 :
                // PLSQL3.g:1:786: QUOTED_STRING
                {
                mQUOTED_STRING(); if (state.failed) return ;

                }
                break;
            case 120 :
                // PLSQL3.g:1:800: ID
                {
                mID(); if (state.failed) return ;

                }
                break;
            case 121 :
                // PLSQL3.g:1:803: SEMI
                {
                mSEMI(); if (state.failed) return ;

                }
                break;
            case 122 :
                // PLSQL3.g:1:808: COLON
                {
                mCOLON(); if (state.failed) return ;

                }
                break;
            case 123 :
                // PLSQL3.g:1:814: DOUBLEDOT
                {
                mDOUBLEDOT(); if (state.failed) return ;

                }
                break;
            case 124 :
                // PLSQL3.g:1:824: DOT
                {
                mDOT(); if (state.failed) return ;

                }
                break;
            case 125 :
                // PLSQL3.g:1:828: COMMA
                {
                mCOMMA(); if (state.failed) return ;

                }
                break;
            case 126 :
                // PLSQL3.g:1:834: EXPONENT
                {
                mEXPONENT(); if (state.failed) return ;

                }
                break;
            case 127 :
                // PLSQL3.g:1:843: ASTERISK
                {
                mASTERISK(); if (state.failed) return ;

                }
                break;
            case 128 :
                // PLSQL3.g:1:852: AT_SIGN
                {
                mAT_SIGN(); if (state.failed) return ;

                }
                break;
            case 129 :
                // PLSQL3.g:1:860: RPAREN
                {
                mRPAREN(); if (state.failed) return ;

                }
                break;
            case 130 :
                // PLSQL3.g:1:867: LPAREN
                {
                mLPAREN(); if (state.failed) return ;

                }
                break;
            case 131 :
                // PLSQL3.g:1:874: RBRACK
                {
                mRBRACK(); if (state.failed) return ;

                }
                break;
            case 132 :
                // PLSQL3.g:1:881: LBRACK
                {
                mLBRACK(); if (state.failed) return ;

                }
                break;
            case 133 :
                // PLSQL3.g:1:888: PLUS
                {
                mPLUS(); if (state.failed) return ;

                }
                break;
            case 134 :
                // PLSQL3.g:1:893: MINUS
                {
                mMINUS(); if (state.failed) return ;

                }
                break;
            case 135 :
                // PLSQL3.g:1:899: DIVIDE
                {
                mDIVIDE(); if (state.failed) return ;

                }
                break;
            case 136 :
                // PLSQL3.g:1:906: EQ
                {
                mEQ(); if (state.failed) return ;

                }
                break;
            case 137 :
                // PLSQL3.g:1:909: PERCENTAGE
                {
                mPERCENTAGE(); if (state.failed) return ;

                }
                break;
            case 138 :
                // PLSQL3.g:1:920: LLABEL
                {
                mLLABEL(); if (state.failed) return ;

                }
                break;
            case 139 :
                // PLSQL3.g:1:927: RLABEL
                {
                mRLABEL(); if (state.failed) return ;

                }
                break;
            case 140 :
                // PLSQL3.g:1:934: ASSIGN
                {
                mASSIGN(); if (state.failed) return ;

                }
                break;
            case 141 :
                // PLSQL3.g:1:941: ARROW
                {
                mARROW(); if (state.failed) return ;

                }
                break;
            case 142 :
                // PLSQL3.g:1:947: VERTBAR
                {
                mVERTBAR(); if (state.failed) return ;

                }
                break;
            case 143 :
                // PLSQL3.g:1:955: DOUBLEVERTBAR
                {
                mDOUBLEVERTBAR(); if (state.failed) return ;

                }
                break;
            case 144 :
                // PLSQL3.g:1:969: NOT_EQ
                {
                mNOT_EQ(); if (state.failed) return ;

                }
                break;
            case 145 :
                // PLSQL3.g:1:976: LTH
                {
                mLTH(); if (state.failed) return ;

                }
                break;
            case 146 :
                // PLSQL3.g:1:980: LEQ
                {
                mLEQ(); if (state.failed) return ;

                }
                break;
            case 147 :
                // PLSQL3.g:1:984: GTH
                {
                mGTH(); if (state.failed) return ;

                }
                break;
            case 148 :
                // PLSQL3.g:1:988: GEQ
                {
                mGEQ(); if (state.failed) return ;

                }
                break;
            case 149 :
                // PLSQL3.g:1:992: NUMBER
                {
                mNUMBER(); if (state.failed) return ;

                }
                break;
            case 150 :
                // PLSQL3.g:1:999: QUOTE
                {
                mQUOTE(); if (state.failed) return ;

                }
                break;
            case 151 :
                // PLSQL3.g:1:1005: WS
                {
                mWS(); if (state.failed) return ;

                }
                break;
            case 152 :
                // PLSQL3.g:1:1008: SL_COMMENT
                {
                mSL_COMMENT(); if (state.failed) return ;

                }
                break;
            case 153 :
                // PLSQL3.g:1:1019: ML_COMMENT
                {
                mML_COMMENT(); if (state.failed) return ;

                }
                break;
            case 154 :
                // PLSQL3.g:1:1030: TYPE_ATTR
                {
                mTYPE_ATTR(); if (state.failed) return ;

                }
                break;
            case 155 :
                // PLSQL3.g:1:1040: ROWTYPE_ATTR
                {
                mROWTYPE_ATTR(); if (state.failed) return ;

                }
                break;
            case 156 :
                // PLSQL3.g:1:1053: NOTFOUND_ATTR
                {
                mNOTFOUND_ATTR(); if (state.failed) return ;

                }
                break;
            case 157 :
                // PLSQL3.g:1:1067: FOUND_ATTR
                {
                mFOUND_ATTR(); if (state.failed) return ;

                }
                break;
            case 158 :
                // PLSQL3.g:1:1078: ISOPEN_ATTR
                {
                mISOPEN_ATTR(); if (state.failed) return ;

                }
                break;
            case 159 :
                // PLSQL3.g:1:1090: ROWCOUNT_ATTR
                {
                mROWCOUNT_ATTR(); if (state.failed) return ;

                }
                break;
            case 160 :
                // PLSQL3.g:1:1104: BULK_ROWCOUNT_ATTR
                {
                mBULK_ROWCOUNT_ATTR(); if (state.failed) return ;

                }
                break;
            case 161 :
                // PLSQL3.g:1:1123: CHARSET_ATTR
                {
                mCHARSET_ATTR(); if (state.failed) return ;

                }
                break;

        }

    }

    // $ANTLR start synpred1_PLSQL3
    public final void synpred1_PLSQL3_fragment() throws RecognitionException {   
        // PLSQL3.g:1553:5: ( N POINT N )
        // PLSQL3.g:1553:7: N POINT N
        {
        mN(); if (state.failed) return ;
        mPOINT(); if (state.failed) return ;
        mN(); if (state.failed) return ;

        }
    }
    // $ANTLR end synpred1_PLSQL3

    public final boolean synpred1_PLSQL3() {
        state.backtracking++;
        int start = input.mark();
        try {
            synpred1_PLSQL3_fragment(); // can never throw exception
        } catch (RecognitionException re) {
            System.err.println("impossible: "+re);
        }
        boolean success = !state.failed;
        input.rewind(start);
        state.backtracking--;
        state.failed=false;
        return success;
    }


    protected DFA6 dfa6 = new DFA6(this);
    protected DFA14 dfa14 = new DFA14(this);
    static final String DFA6_eotS =
        "\1\uffff\1\4\1\uffff\1\4\2\uffff";
    static final String DFA6_eofS =
        "\6\uffff";
    static final String DFA6_minS =
        "\2\56\1\uffff\1\56\2\uffff";
    static final String DFA6_maxS =
        "\2\71\1\uffff\1\71\2\uffff";
    static final String DFA6_acceptS =
        "\2\uffff\1\2\1\uffff\1\3\1\1";
    static final String DFA6_specialS =
        "\1\uffff\1\0\1\uffff\1\1\2\uffff}>";
    static final String[] DFA6_transitionS = {
            "\1\2\1\uffff\12\1",
            "\1\5\1\uffff\12\3",
            "",
            "\1\5\1\uffff\12\3",
            "",
            ""
    };

    static final short[] DFA6_eot = DFA.unpackEncodedString(DFA6_eotS);
    static final short[] DFA6_eof = DFA.unpackEncodedString(DFA6_eofS);
    static final char[] DFA6_min = DFA.unpackEncodedStringToUnsignedChars(DFA6_minS);
    static final char[] DFA6_max = DFA.unpackEncodedStringToUnsignedChars(DFA6_maxS);
    static final short[] DFA6_accept = DFA.unpackEncodedString(DFA6_acceptS);
    static final short[] DFA6_special = DFA.unpackEncodedString(DFA6_specialS);
    static final short[][] DFA6_transition;

    static {
        int numStates = DFA6_transitionS.length;
        DFA6_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA6_transition[i] = DFA.unpackEncodedString(DFA6_transitionS[i]);
        }
    }

    class DFA6 extends DFA {

        public DFA6(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 6;
            this.eot = DFA6_eot;
            this.eof = DFA6_eof;
            this.min = DFA6_min;
            this.max = DFA6_max;
            this.accept = DFA6_accept;
            this.special = DFA6_special;
            this.transition = DFA6_transition;
        }
        public String getDescription() {
            return "1553:3: ( ( N POINT N )=> N POINT N | POINT N | N )";
        }
        public int specialStateTransition(int s, IntStream _input) throws NoViableAltException {
            IntStream input = _input;
        	int _s = s;
            switch ( s ) {
                    case 0 : 
                        int LA6_1 = input.LA(1);

                         
                        int index6_1 = input.index();
                        input.rewind();
                        s = -1;
                        if ( ((LA6_1>='0' && LA6_1<='9')) ) {s = 3;}

                        else if ( (LA6_1=='.') && (synpred1_PLSQL3())) {s = 5;}

                        else s = 4;

                         
                        input.seek(index6_1);
                        if ( s>=0 ) return s;
                        break;
                    case 1 : 
                        int LA6_3 = input.LA(1);

                         
                        int index6_3 = input.index();
                        input.rewind();
                        s = -1;
                        if ( ((LA6_3>='0' && LA6_3<='9')) ) {s = 3;}

                        else if ( (LA6_3=='.') && (synpred1_PLSQL3())) {s = 5;}

                        else s = 4;

                         
                        input.seek(index6_3);
                        if ( s>=0 ) return s;
                        break;
            }
            if (state.backtracking>0) {state.failed=true; return -1;}
            NoViableAltException nvae =
                new NoViableAltException(getDescription(), 6, _s, input);
            error(nvae);
            throw nvae;
        }
    }
    static final String DFA14_eotS =
        "\1\uffff\24\27\1\uffff\1\164\2\uffff\1\166\1\167\1\uffff\1\172\6"+
        "\uffff\1\174\1\176\1\u0080\1\u0088\1\u008b\1\u008e\1\u0090\3\uffff"+
        "\5\27\1\u0098\1\u0099\1\u009a\1\27\1\u009c\1\u00a0\1\u00a1\1\u00a3"+
        "\2\27\1\u00a7\10\27\1\u00b2\26\27\1\u00d8\27\27\35\uffff\7\27\3"+
        "\uffff\1\u00fb\1\uffff\1\u00fe\2\27\2\uffff\1\u0101\1\uffff\1\u0102"+
        "\1\u0103\1\u0104\1\uffff\1\u0105\11\27\1\uffff\1\u0111\10\27\1\u011e"+
        "\24\27\1\u0134\4\27\1\u013b\1\27\1\uffff\4\27\1\u0141\2\27\1\u0144"+
        "\5\27\1\u014a\13\27\1\uffff\4\27\1\u015e\1\u015f\1\u0160\1\27\1"+
        "\uffff\1\27\1\u0164\1\uffff\2\27\5\uffff\3\27\1\u016a\5\27\1\u0170"+
        "\1\27\1\uffff\1\27\1\u0173\12\27\1\uffff\1\u017e\1\27\1\u0180\2"+
        "\27\1\u0183\1\27\1\u0185\2\27\1\u0188\6\27\1\u018f\1\u0190\1\u0191"+
        "\1\u0192\1\uffff\1\27\1\u0194\3\27\1\u0198\1\uffff\2\27\1\u019b"+
        "\1\u019c\1\27\1\uffff\2\27\1\uffff\5\27\1\uffff\1\u01a5\5\27\1\u01ab"+
        "\5\27\1\uffff\6\27\3\uffff\1\u01b9\2\27\1\uffff\1\u01bc\4\27\1\uffff"+
        "\1\u01c1\1\u01c2\3\27\1\uffff\1\u01c6\1\27\1\uffff\4\27\1\u01cc"+
        "\1\u01cd\4\27\1\uffff\1\27\1\uffff\2\27\1\uffff\1\u01d5\1\uffff"+
        "\1\u01d6\1\u01d7\1\uffff\3\27\1\u01db\2\27\4\uffff\1\u01de\1\uffff"+
        "\2\27\1\u01e1\1\uffff\1\27\1\u01e3\2\uffff\2\27\1\u01e6\1\u01e7"+
        "\1\27\1\u01e9\1\27\1\u01eb\1\uffff\1\u01ec\3\27\1\u01f0\1\uffff"+
        "\2\27\1\u01f3\2\27\2\uffff\1\u01f6\2\27\1\u01f9\2\27\1\uffff\2\27"+
        "\1\uffff\1\u01fe\2\27\1\u0201\2\uffff\3\27\1\uffff\1\u0205\1\u0206"+
        "\3\27\2\uffff\4\27\1\u020e\1\u020f\1\27\3\uffff\3\27\1\uffff\1\u0214"+
        "\1\27\1\uffff\1\u0216\1\u0218\1\uffff\1\27\1\uffff\1\27\1\u021b"+
        "\2\uffff\1\27\1\uffff\1\27\2\uffff\1\27\1\u021f\1\27\1\uffff\1\u0221"+
        "\1\u0222\1\uffff\1\u0223\1\u0224\1\uffff\1\27\1\u0226\1\uffff\1"+
        "\u0227\1\27\1\u0229\1\27\1\uffff\2\27\1\uffff\1\u022d\1\27\1\u0231"+
        "\2\uffff\1\u0232\1\u0233\2\27\1\u0236\1\u0237\1\u0238\2\uffff\4"+
        "\27\1\uffff\1\u023d\1\uffff\1\27\1\uffff\2\27\1\uffff\2\27\1\u0244"+
        "\1\uffff\1\27\4\uffff\1\u0246\2\uffff\1\27\1\uffff\3\27\1\uffff"+
        "\3\27\3\uffff\1\u024e\1\u0250\3\uffff\1\u0251\1\u0252\2\27\1\uffff"+
        "\1\27\1\u0256\1\u0257\1\27\1\u0259\1\u025a\1\uffff\1\u025b\1\uffff"+
        "\1\u025c\1\u025d\1\u025e\1\u025f\3\27\1\uffff\1\u0263\3\uffff\1"+
        "\27\1\u0265\1\u0266\2\uffff\1\u0267\7\uffff\3\27\1\uffff\1\27\3"+
        "\uffff\3\27\1\u026f\1\27\1\u0271\1\27\1\uffff\1\27\1\uffff\1\u0274"+
        "\1\u0275\2\uffff";
    static final String DFA14_eofS =
        "\u0276\uffff";
    static final String DFA14_minS =
        "\1\11\1\101\2\106\2\114\1\105\2\101\1\117\1\110\1\101\1\111\5\101"+
        "\1\111\1\116\1\101\1\uffff\1\0\2\uffff\1\75\1\56\1\uffff\1\52\6"+
        "\uffff\1\55\1\52\1\76\1\102\1\74\1\75\1\174\3\uffff\1\105\1\115"+
        "\1\101\1\117\1\123\3\43\1\124\4\43\1\114\1\104\1\43\1\104\1\103"+
        "\1\123\1\107\1\116\2\117\1\111\1\43\1\124\1\114\1\124\1\110\1\101"+
        "\1\103\1\124\1\125\1\123\1\124\1\117\1\105\1\124\2\123\1\101\2\103"+
        "\1\113\1\111\1\101\1\114\1\43\1\102\1\105\1\125\1\101\1\114\1\101"+
        "\1\114\1\101\1\126\1\117\1\116\1\114\1\122\1\117\1\124\1\114\1\123"+
        "\1\116\1\104\1\117\1\111\1\104\1\126\16\uffff\1\117\16\uffff\1\101"+
        "\1\116\1\115\1\122\1\102\2\105\3\uffff\1\43\1\uffff\1\43\2\105\2"+
        "\uffff\1\43\1\uffff\3\43\1\uffff\1\43\1\105\1\123\1\105\1\111\1"+
        "\127\1\101\1\114\1\102\1\114\1\uffff\1\43\1\101\1\114\1\102\1\111"+
        "\1\101\1\117\1\122\1\101\1\43\1\103\2\105\1\102\1\124\1\117\1\125"+
        "\1\116\1\114\1\110\1\111\1\137\1\103\1\117\1\107\1\113\1\107\1\113"+
        "\1\120\1\105\1\43\1\123\1\114\1\117\1\125\1\43\1\114\1\uffff\1\114"+
        "\1\116\1\105\1\114\1\43\1\105\1\122\1\43\1\122\1\105\1\101\1\103"+
        "\1\123\1\43\1\115\2\103\1\125\1\114\1\125\1\105\1\127\1\117\1\101"+
        "\1\111\1\127\2\124\2\105\3\43\1\122\1\uffff\1\107\1\43\1\uffff\1"+
        "\130\1\122\5\uffff\1\120\1\125\1\124\1\43\1\106\1\116\1\105\1\122"+
        "\1\105\1\43\1\105\1\uffff\1\111\1\43\1\105\2\122\1\117\1\122\1\102"+
        "\1\103\1\125\1\101\1\115\1\uffff\1\43\1\124\1\43\1\114\1\111\1\43"+
        "\1\120\1\43\2\105\1\43\1\124\1\111\1\105\1\122\1\115\1\101\4\43"+
        "\1\uffff\1\105\1\43\2\122\1\104\1\43\1\uffff\1\102\1\105\2\43\1"+
        "\114\1\uffff\1\103\1\124\1\uffff\1\105\1\120\2\124\1\105\1\uffff"+
        "\1\43\2\110\1\105\1\101\1\123\1\43\1\111\1\125\1\116\1\124\1\116"+
        "\1\103\1\105\1\101\1\103\1\124\1\116\1\103\3\uffff\1\43\1\105\1"+
        "\123\1\uffff\1\43\2\124\2\123\1\uffff\2\43\1\105\1\131\1\101\1\uffff"+
        "\1\43\1\124\1\uffff\1\122\1\111\1\101\1\116\2\43\1\110\1\114\1\122"+
        "\1\101\1\uffff\1\105\1\uffff\1\105\1\116\1\uffff\1\43\1\uffff\2"+
        "\43\1\uffff\1\111\1\116\1\104\1\43\1\101\1\107\4\uffff\1\43\1\uffff"+
        "\1\104\1\116\1\43\1\uffff\1\101\1\43\2\uffff\1\111\1\124\2\43\1"+
        "\117\1\43\1\111\1\43\1\uffff\1\43\1\101\1\123\1\102\1\43\1\uffff"+
        "\1\104\1\105\1\43\1\105\1\107\2\uffff\1\43\1\116\1\124\1\43\2\124"+
        "\1\uffff\1\122\1\105\1\uffff\1\43\2\111\1\43\2\uffff\1\116\1\137"+
        "\1\116\1\uffff\2\43\1\103\1\114\1\101\2\uffff\1\101\1\124\1\105"+
        "\1\114\2\43\1\103\3\uffff\1\126\1\124\1\125\1\uffff\1\43\1\105\1"+
        "\uffff\2\43\1\uffff\1\103\1\uffff\1\116\1\43\2\uffff\1\111\1\uffff"+
        "\1\117\2\uffff\1\122\1\43\1\105\1\uffff\2\43\1\uffff\2\43\1\uffff"+
        "\1\124\1\43\1\uffff\1\43\1\105\1\43\1\103\1\uffff\1\117\1\126\1"+
        "\uffff\1\43\1\104\1\43\2\uffff\2\43\1\114\1\122\3\43\2\uffff\1\124"+
        "\2\105\1\122\1\uffff\1\43\1\uffff\1\116\1\uffff\1\113\1\124\1\uffff"+
        "\2\116\1\43\1\uffff\1\114\4\uffff\1\43\2\uffff\1\122\1\uffff\1\124"+
        "\1\116\1\105\1\uffff\1\116\1\114\1\117\3\uffff\2\43\3\uffff\2\43"+
        "\1\107\1\105\1\uffff\1\107\2\43\1\124\2\43\1\uffff\1\43\1\uffff"+
        "\4\43\1\124\1\117\1\125\1\uffff\1\43\3\uffff\1\105\2\43\2\uffff"+
        "\1\43\7\uffff\1\105\1\101\1\102\1\uffff\1\122\3\uffff\1\107\1\124"+
        "\1\114\1\43\1\105\1\43\1\105\1\uffff\1\122\1\uffff\2\43\2\uffff";
    static final String DFA14_maxS =
        "\1\174\1\122\1\125\1\123\1\124\1\130\1\131\1\126\1\117\1\122\1\111"+
        "\1\122\2\117\1\122\1\124\1\125\1\101\1\117\1\122\1\101\1\uffff\1"+
        "\uffff\2\uffff\1\75\1\71\1\uffff\1\52\6\uffff\1\55\1\52\1\76\1\124"+
        "\2\76\1\174\3\uffff\1\105\1\116\1\101\1\117\1\123\3\137\1\124\4"+
        "\137\1\114\1\131\1\137\1\104\1\111\1\123\1\124\1\116\2\117\1\111"+
        "\1\137\1\127\1\115\1\124\1\114\1\101\1\123\1\124\1\125\1\123\1\124"+
        "\1\117\1\111\1\124\2\123\1\117\1\103\1\117\1\113\1\127\1\124\1\127"+
        "\1\137\1\102\1\105\1\125\1\101\1\124\1\101\1\114\1\101\1\126\1\117"+
        "\1\116\1\114\1\122\1\117\1\124\1\122\1\123\1\116\1\104\1\117\1\111"+
        "\1\104\1\126\16\uffff\1\117\16\uffff\1\101\1\123\1\115\1\122\1\102"+
        "\2\105\3\uffff\1\137\1\uffff\1\137\2\105\2\uffff\1\137\1\uffff\3"+
        "\137\1\uffff\1\137\1\114\1\123\2\111\1\127\1\101\1\114\1\102\1\114"+
        "\1\uffff\1\137\1\101\1\114\1\105\1\125\1\101\1\117\1\122\1\101\1"+
        "\137\1\103\2\105\1\102\1\124\1\117\1\125\1\122\1\114\1\110\1\111"+
        "\1\137\1\103\1\117\1\107\1\113\1\107\1\113\1\120\1\105\1\137\1\123"+
        "\1\114\1\117\1\125\1\137\1\114\1\uffff\1\114\1\116\1\105\1\114\1"+
        "\137\1\105\1\122\1\137\1\122\1\105\1\101\1\103\1\123\1\137\1\115"+
        "\2\103\1\125\1\114\1\125\1\105\1\127\1\121\1\101\1\111\1\127\2\124"+
        "\1\105\1\111\3\137\1\122\1\uffff\1\122\1\137\1\uffff\1\130\1\122"+
        "\5\uffff\1\120\1\125\1\124\1\137\1\106\1\116\1\105\1\122\1\105\1"+
        "\137\1\105\1\uffff\1\111\1\137\1\105\2\122\1\117\1\122\1\102\1\103"+
        "\1\125\1\101\1\115\1\uffff\1\137\1\124\1\137\1\114\1\111\1\137\1"+
        "\120\1\137\2\105\1\137\1\124\1\111\1\105\1\122\1\115\1\101\4\137"+
        "\1\uffff\1\105\1\137\2\122\1\104\1\137\1\uffff\1\102\1\105\2\137"+
        "\1\114\1\uffff\1\103\1\124\1\uffff\1\105\1\120\2\124\1\105\1\uffff"+
        "\1\137\2\110\1\105\1\101\1\123\1\137\1\111\1\125\1\116\1\124\1\116"+
        "\1\124\1\105\1\101\1\103\1\124\1\116\1\103\3\uffff\1\137\1\105\1"+
        "\123\1\uffff\1\137\2\124\2\123\1\uffff\2\137\1\105\1\131\1\101\1"+
        "\uffff\1\137\1\124\1\uffff\1\122\1\111\1\101\1\116\2\137\1\110\1"+
        "\114\1\122\1\101\1\uffff\1\105\1\uffff\1\105\1\116\1\uffff\1\137"+
        "\1\uffff\2\137\1\uffff\1\111\1\116\1\104\1\137\1\101\1\107\4\uffff"+
        "\1\137\1\uffff\1\104\1\116\1\137\1\uffff\1\101\1\137\2\uffff\1\111"+
        "\1\124\2\137\1\117\1\137\1\111\1\137\1\uffff\1\137\1\101\1\123\1"+
        "\102\1\137\1\uffff\1\104\1\105\1\137\1\105\1\107\2\uffff\1\137\1"+
        "\116\1\124\1\137\2\124\1\uffff\1\122\1\105\1\uffff\1\137\2\111\1"+
        "\137\2\uffff\1\116\1\137\1\116\1\uffff\2\137\1\103\1\114\1\101\2"+
        "\uffff\1\101\1\124\1\105\1\114\2\137\1\103\3\uffff\1\126\1\124\1"+
        "\125\1\uffff\1\137\1\105\1\uffff\2\137\1\uffff\1\103\1\uffff\1\116"+
        "\1\137\2\uffff\1\111\1\uffff\1\117\2\uffff\1\122\1\137\1\105\1\uffff"+
        "\2\137\1\uffff\2\137\1\uffff\1\124\1\137\1\uffff\1\137\1\105\1\137"+
        "\1\103\1\uffff\1\117\1\126\1\uffff\1\137\1\111\1\137\2\uffff\2\137"+
        "\1\114\1\122\3\137\2\uffff\1\124\2\105\1\122\1\uffff\1\137\1\uffff"+
        "\1\116\1\uffff\1\113\1\124\1\uffff\2\116\1\137\1\uffff\1\114\4\uffff"+
        "\1\137\2\uffff\1\122\1\uffff\1\124\1\116\1\105\1\uffff\1\116\1\114"+
        "\1\117\3\uffff\2\137\3\uffff\2\137\1\107\1\105\1\uffff\1\107\2\137"+
        "\1\124\2\137\1\uffff\1\137\1\uffff\4\137\1\124\1\117\1\125\1\uffff"+
        "\1\137\3\uffff\1\105\2\137\2\uffff\1\137\7\uffff\1\105\1\101\1\102"+
        "\1\uffff\1\122\3\uffff\1\107\1\124\1\114\1\137\1\105\1\137\1\105"+
        "\1\uffff\1\122\1\uffff\2\137\2\uffff";
    static final String DFA14_acceptS =
        "\25\uffff\1\167\1\uffff\1\170\1\171\2\uffff\1\175\1\uffff\1\u0080"+
        "\1\u0081\1\u0082\1\u0083\1\u0084\1\u0085\7\uffff\1\u0090\1\u0095"+
        "\1\u0097\107\uffff\1\u0096\1\u008c\1\172\1\174\1\173\1\176\1\177"+
        "\1\u0098\1\u0086\1\u0099\1\u0087\1\u008d\1\u0088\1\u009a\1\uffff"+
        "\1\u009c\1\u009d\1\u009e\1\u00a0\1\u00a1\1\u0089\1\u008a\1\u0092"+
        "\1\u0091\1\u008b\1\u0094\1\u0093\1\u008f\1\u008e\7\uffff\1\2\1\71"+
        "\1\115\1\uffff\1\3\3\uffff\1\65\1\101\1\uffff\1\4\3\uffff\1\136"+
        "\12\uffff\1\73\45\uffff\1\34\42\uffff\1\156\2\uffff\1\37\2\uffff"+
        "\1\126\1\106\1\131\1\141\1\5\13\uffff\1\10\14\uffff\1\27\25\uffff"+
        "\1\31\6\uffff\1\147\5\uffff\1\46\2\uffff\1\133\5\uffff\1\77\23\uffff"+
        "\1\44\1\60\1\135\3\uffff\1\107\5\uffff\1\102\5\uffff\1\57\2\uffff"+
        "\1\11\12\uffff\1\127\1\uffff\1\33\2\uffff\1\15\1\uffff\1\16\2\uffff"+
        "\1\35\6\uffff\1\30\1\145\1\155\1\125\1\uffff\1\42\3\uffff\1\122"+
        "\2\uffff\1\74\1\75\10\uffff\1\110\5\uffff\1\146\5\uffff\1\u009b"+
        "\1\u009f\6\uffff\1\124\2\uffff\1\72\4\uffff\1\154\1\6\3\uffff\1"+
        "\62\5\uffff\1\51\1\61\7\uffff\1\121\1\116\1\166\3\uffff\1\134\2"+
        "\uffff\1\161\2\uffff\1\63\1\uffff\1\70\2\uffff\1\117\1\150\1\uffff"+
        "\1\41\1\uffff\1\76\1\144\3\uffff\1\114\2\uffff\1\112\2\uffff\1\1"+
        "\2\uffff\1\100\4\uffff\1\142\2\uffff\1\137\3\uffff\1\130\1\24\7"+
        "\uffff\1\140\1\43\4\uffff\1\160\1\uffff\1\162\1\uffff\1\163\2\uffff"+
        "\1\103\3\uffff\1\143\1\uffff\1\64\1\105\1\123\1\111\1\uffff\1\120"+
        "\1\153\1\uffff\1\36\3\uffff\1\132\3\uffff\1\32\1\25\1\22\2\uffff"+
        "\1\12\1\13\1\26\4\uffff\1\157\6\uffff\1\47\1\uffff\1\7\7\uffff\1"+
        "\54\1\uffff\1\52\1\104\1\23\3\uffff\1\165\1\40\1\uffff\1\67\1\50"+
        "\1\55\1\45\1\113\1\14\1\151\3\uffff\1\53\1\uffff\1\66\1\164\1\152"+
        "\7\uffff\1\56\1\uffff\1\20\2\uffff\1\21\1\17";
    static final String DFA14_specialS =
        "\26\uffff\1\0\u025f\uffff}>";
    static final String[] DFA14_transitionS = {
            "\2\54\2\uffff\1\54\22\uffff\1\54\1\52\1\27\2\uffff\1\46\1\uffff"+
            "\1\26\1\37\1\36\1\34\1\42\1\33\1\43\1\32\1\44\12\53\1\31\1\30"+
            "\1\47\1\45\1\50\1\uffff\1\35\1\4\1\6\1\1\1\10\1\5\1\20\1\11"+
            "\1\24\1\3\2\27\1\14\1\22\1\7\1\2\1\13\1\27\1\15\1\17\1\16\1"+
            "\23\1\21\1\12\3\27\1\41\1\uffff\1\40\1\52\17\uffff\1\25\15\uffff"+
            "\1\51",
            "\1\61\6\uffff\1\57\3\uffff\1\60\2\uffff\1\56\2\uffff\1\55",
            "\1\63\7\uffff\1\64\3\uffff\1\62\2\uffff\1\65",
            "\1\70\7\uffff\1\67\4\uffff\1\66",
            "\1\72\1\uffff\1\73\4\uffff\1\71\1\74",
            "\1\77\1\uffff\1\75\11\uffff\1\76",
            "\1\100\1\104\2\uffff\1\101\2\uffff\1\103\2\uffff\1\102\11\uffff"+
            "\1\105",
            "\1\110\1\uffff\1\111\13\uffff\1\106\5\uffff\1\107\1\112",
            "\1\114\3\uffff\1\113\3\uffff\1\116\5\uffff\1\115",
            "\1\117\2\uffff\1\120",
            "\1\121\1\122",
            "\1\126\12\uffff\1\124\2\uffff\1\123\2\uffff\1\125",
            "\1\130\5\uffff\1\127",
            "\1\131\3\uffff\1\132\11\uffff\1\133",
            "\1\135\6\uffff\1\136\6\uffff\1\134\2\uffff\1\137",
            "\1\145\3\uffff\1\141\2\uffff\1\144\4\uffff\1\140\3\uffff\1"+
            "\143\2\uffff\1\142",
            "\1\150\3\uffff\1\153\6\uffff\1\146\2\uffff\1\151\2\uffff\1"+
            "\152\2\uffff\1\147",
            "\1\154",
            "\1\156\2\uffff\1\155\2\uffff\1\157",
            "\1\161\1\uffff\1\162\1\uffff\1\160",
            "\1\163",
            "",
            "\0\25",
            "",
            "",
            "\1\165",
            "\1\170\1\uffff\12\53",
            "",
            "\1\171",
            "",
            "",
            "",
            "",
            "",
            "",
            "\1\173",
            "\1\175",
            "\1\177",
            "\1\u0086\1\u0087\2\uffff\1\u0084\2\uffff\1\u0085\4\uffff\1"+
            "\u0083\3\uffff\1\u0082\1\uffff\1\u0081",
            "\1\u0089\1\u008a\1\52",
            "\1\u008d\1\u008c",
            "\1\u008f",
            "",
            "",
            "",
            "\1\u0091",
            "\1\u0093\1\u0092",
            "\1\u0094",
            "\1\u0095",
            "\1\u0096",
            "\2\27\13\uffff\12\27\7\uffff\3\27\1\u0097\26\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u009b",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\3\27\1\u009e\16\27\1\u009f\1\u009d"+
            "\6\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\2\27\1\u00a2\27\27\4\uffff\1\27",
            "\1\u00a4",
            "\1\u00a5\24\uffff\1\u00a6",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u00a8",
            "\1\u00a9\5\uffff\1\u00aa",
            "\1\u00ab",
            "\1\u00ac\14\uffff\1\u00ad",
            "\1\u00ae",
            "\1\u00af",
            "\1\u00b0",
            "\1\u00b1",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u00b3\2\uffff\1\u00b4",
            "\1\u00b5\1\u00b6",
            "\1\u00b7",
            "\1\u00b8\3\uffff\1\u00b9",
            "\1\u00ba",
            "\1\u00bc\2\uffff\1\u00bb\5\uffff\1\u00be\6\uffff\1\u00bd",
            "\1\u00bf",
            "\1\u00c0",
            "\1\u00c1",
            "\1\u00c2",
            "\1\u00c3",
            "\1\u00c4\3\uffff\1\u00c5",
            "\1\u00c6",
            "\1\u00c7",
            "\1\u00c8",
            "\1\u00cb\7\uffff\1\u00ca\5\uffff\1\u00c9",
            "\1\u00cc",
            "\1\u00ce\12\uffff\1\u00cd\1\u00cf",
            "\1\u00d0",
            "\1\u00d2\15\uffff\1\u00d1",
            "\1\u00d3\1\uffff\1\u00d4\20\uffff\1\u00d5",
            "\1\u00d7\12\uffff\1\u00d6",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u00d9",
            "\1\u00da",
            "\1\u00db",
            "\1\u00dc",
            "\1\u00de\7\uffff\1\u00dd",
            "\1\u00df",
            "\1\u00e0",
            "\1\u00e1",
            "\1\u00e2",
            "\1\u00e3",
            "\1\u00e4",
            "\1\u00e5",
            "\1\u00e6",
            "\1\u00e7",
            "\1\u00e8",
            "\1\u00ea\5\uffff\1\u00e9",
            "\1\u00eb",
            "\1\u00ec",
            "\1\u00ed",
            "\1\u00ee",
            "\1\u00ef",
            "\1\u00f0",
            "\1\u00f1",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "\1\u00f2",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "\1\u00f3",
            "\1\u00f5\4\uffff\1\u00f4",
            "\1\u00f6",
            "\1\u00f7",
            "\1\u00f8",
            "\1\u00f9",
            "\1\u00fa",
            "",
            "",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "\2\27\13\uffff\12\27\7\uffff\4\27\1\u00fc\11\27\1\u00fd\13"+
            "\27\4\uffff\1\27",
            "\1\u00ff",
            "\1\u0100",
            "",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u0106\6\uffff\1\u0107",
            "\1\u0108",
            "\1\u0109\3\uffff\1\u010a",
            "\1\u010b",
            "\1\u010c",
            "\1\u010d",
            "\1\u010e",
            "\1\u010f",
            "\1\u0110",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u0112",
            "\1\u0113",
            "\1\u0114\2\uffff\1\u0115",
            "\1\u0117\13\uffff\1\u0116",
            "\1\u0118",
            "\1\u0119",
            "\1\u011a",
            "\1\u011b",
            "\2\27\13\uffff\12\27\7\uffff\10\27\1\u011d\2\27\1\u011c\16"+
            "\27\4\uffff\1\27",
            "\1\u011f",
            "\1\u0120",
            "\1\u0121",
            "\1\u0122",
            "\1\u0123",
            "\1\u0124",
            "\1\u0125",
            "\1\u0126\3\uffff\1\u0127",
            "\1\u0128",
            "\1\u0129",
            "\1\u012a",
            "\1\u012b",
            "\1\u012c",
            "\1\u012d",
            "\1\u012e",
            "\1\u012f",
            "\1\u0130",
            "\1\u0131",
            "\1\u0132",
            "\1\u0133",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u0135",
            "\1\u0136",
            "\1\u0137",
            "\1\u0138",
            "\2\27\13\uffff\12\27\7\uffff\10\27\1\u0139\11\27\1\u013a\7"+
            "\27\4\uffff\1\27",
            "\1\u013c",
            "",
            "\1\u013d",
            "\1\u013e",
            "\1\u013f",
            "\1\u0140",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u0142",
            "\1\u0143",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u0145",
            "\1\u0146",
            "\1\u0147",
            "\1\u0148",
            "\1\u0149",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u014b",
            "\1\u014c",
            "\1\u014d",
            "\1\u014e",
            "\1\u014f",
            "\1\u0150",
            "\1\u0151",
            "\1\u0152",
            "\1\u0154\1\uffff\1\u0153",
            "\1\u0155",
            "\1\u0156",
            "\1\u0157",
            "\1\u0158",
            "\1\u0159",
            "\1\u015a",
            "\1\u015c\3\uffff\1\u015b",
            "\2\27\13\uffff\12\27\7\uffff\1\u015d\31\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u0161",
            "",
            "\1\u0162\12\uffff\1\u0163",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "\1\u0165",
            "\1\u0166",
            "",
            "",
            "",
            "",
            "",
            "\1\u0167",
            "\1\u0168",
            "\1\u0169",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u016b",
            "\1\u016c",
            "\1\u016d",
            "\1\u016e",
            "\1\u016f",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u0171",
            "",
            "\1\u0172",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u0174",
            "\1\u0175",
            "\1\u0176",
            "\1\u0177",
            "\1\u0178",
            "\1\u0179",
            "\1\u017a",
            "\1\u017b",
            "\1\u017c",
            "\1\u017d",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u017f",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u0181",
            "\1\u0182",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u0184",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u0186",
            "\1\u0187",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u0189",
            "\1\u018a",
            "\1\u018b",
            "\1\u018c",
            "\1\u018d",
            "\1\u018e",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "\1\u0193",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u0195",
            "\1\u0196",
            "\1\u0197",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "\1\u0199",
            "\1\u019a",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u019d",
            "",
            "\1\u019e",
            "\1\u019f",
            "",
            "\1\u01a0",
            "\1\u01a1",
            "\1\u01a2",
            "\1\u01a3",
            "\1\u01a4",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u01a6",
            "\1\u01a7",
            "\1\u01a8",
            "\1\u01a9",
            "\1\u01aa",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u01ac",
            "\1\u01ad",
            "\1\u01ae",
            "\1\u01af",
            "\1\u01b0",
            "\1\u01b2\20\uffff\1\u01b1",
            "\1\u01b3",
            "\1\u01b4",
            "\1\u01b5",
            "\1\u01b6",
            "\1\u01b7",
            "\1\u01b8",
            "",
            "",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u01ba",
            "\1\u01bb",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u01bd",
            "\1\u01be",
            "\1\u01bf",
            "\1\u01c0",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u01c3",
            "\1\u01c4",
            "\1\u01c5",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u01c7",
            "",
            "\1\u01c8",
            "\1\u01c9",
            "\1\u01ca",
            "\1\u01cb",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u01ce",
            "\1\u01cf",
            "\1\u01d0",
            "\1\u01d1",
            "",
            "\1\u01d2",
            "",
            "\1\u01d3",
            "\1\u01d4",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "\1\u01d8",
            "\1\u01d9",
            "\1\u01da",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u01dc",
            "\1\u01dd",
            "",
            "",
            "",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "\1\u01df",
            "\1\u01e0",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "\1\u01e2",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "",
            "\1\u01e4",
            "\1\u01e5",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u01e8",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u01ea",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u01ed",
            "\1\u01ee",
            "\1\u01ef",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "\1\u01f1",
            "\1\u01f2",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u01f4",
            "\1\u01f5",
            "",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u01f7",
            "\1\u01f8",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u01fa",
            "\1\u01fb",
            "",
            "\1\u01fc",
            "\1\u01fd",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u01ff",
            "\1\u0200",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "",
            "\1\u0202",
            "\1\u0203",
            "\1\u0204",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u0207",
            "\1\u0208",
            "\1\u0209",
            "",
            "",
            "\1\u020a",
            "\1\u020b",
            "\1\u020c",
            "\1\u020d",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u0210",
            "",
            "",
            "",
            "\1\u0211",
            "\1\u0212",
            "\1\u0213",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u0215",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\10\27\1\u0217\21\27\4\uffff\1"+
            "\27",
            "",
            "\1\u0219",
            "",
            "\1\u021a",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "",
            "\1\u021c",
            "",
            "\1\u021d",
            "",
            "",
            "\1\u021e",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u0220",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "\1\u0225",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u0228",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u022a",
            "",
            "\1\u022b",
            "\1\u022c",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u0230\1\uffff\1\u022f\2\uffff\1\u022e",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u0234",
            "\1\u0235",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "",
            "\1\u0239",
            "\1\u023a",
            "\1\u023b",
            "\1\u023c",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "\1\u023e",
            "",
            "\1\u023f",
            "\1\u0240",
            "",
            "\1\u0241",
            "\1\u0242",
            "\2\27\13\uffff\2\27\1\u0243\7\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "\1\u0245",
            "",
            "",
            "",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "",
            "\1\u0247",
            "",
            "\1\u0248",
            "\1\u0249",
            "\1\u024a",
            "",
            "\1\u024b",
            "\1\u024c",
            "\1\u024d",
            "",
            "",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\2\27\1\u024f\7\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u0253",
            "\1\u0254",
            "",
            "\1\u0255",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u0258",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u0260",
            "\1\u0261",
            "\1\u0262",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "",
            "",
            "\1\u0264",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "\1\u0268",
            "\1\u0269",
            "\1\u026a",
            "",
            "\1\u026b",
            "",
            "",
            "",
            "\1\u026c",
            "\1\u026d",
            "\1\u026e",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u0270",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\1\u0272",
            "",
            "\1\u0273",
            "",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "\2\27\13\uffff\12\27\7\uffff\32\27\4\uffff\1\27",
            "",
            ""
    };

    static final short[] DFA14_eot = DFA.unpackEncodedString(DFA14_eotS);
    static final short[] DFA14_eof = DFA.unpackEncodedString(DFA14_eofS);
    static final char[] DFA14_min = DFA.unpackEncodedStringToUnsignedChars(DFA14_minS);
    static final char[] DFA14_max = DFA.unpackEncodedStringToUnsignedChars(DFA14_maxS);
    static final short[] DFA14_accept = DFA.unpackEncodedString(DFA14_acceptS);
    static final short[] DFA14_special = DFA.unpackEncodedString(DFA14_specialS);
    static final short[][] DFA14_transition;

    static {
        int numStates = DFA14_transitionS.length;
        DFA14_transition = new short[numStates][];
        for (int i=0; i<numStates; i++) {
            DFA14_transition[i] = DFA.unpackEncodedString(DFA14_transitionS[i]);
        }
    }

    class DFA14 extends DFA {

        public DFA14(BaseRecognizer recognizer) {
            this.recognizer = recognizer;
            this.decisionNumber = 14;
            this.eot = DFA14_eot;
            this.eof = DFA14_eof;
            this.min = DFA14_min;
            this.max = DFA14_max;
            this.accept = DFA14_accept;
            this.special = DFA14_special;
            this.transition = DFA14_transition;
        }
        public String getDescription() {
            return "1:1: Tokens : ( T__50 | T__51 | T__52 | T__53 | T__54 | T__55 | T__56 | T__57 | T__58 | T__59 | T__60 | T__61 | T__62 | T__63 | T__64 | T__65 | T__66 | T__67 | T__68 | T__69 | T__70 | T__71 | T__72 | T__73 | T__74 | T__75 | T__76 | T__77 | T__78 | T__79 | T__80 | T__81 | T__82 | T__83 | T__84 | T__85 | T__86 | T__87 | T__88 | T__89 | T__90 | T__91 | T__92 | T__93 | T__94 | T__95 | T__96 | T__97 | T__98 | T__99 | T__100 | T__101 | T__102 | T__103 | T__104 | T__105 | T__106 | T__107 | T__108 | T__109 | T__110 | T__111 | T__112 | T__113 | T__114 | T__115 | T__116 | T__117 | T__118 | T__119 | T__120 | T__121 | T__122 | T__123 | T__124 | T__125 | T__126 | T__127 | T__128 | T__129 | T__130 | T__131 | T__132 | T__133 | T__134 | T__135 | T__136 | T__137 | T__138 | T__139 | T__140 | T__141 | T__142 | T__143 | T__144 | T__145 | T__146 | T__147 | T__148 | T__149 | T__150 | T__151 | T__152 | T__153 | T__154 | T__155 | T__156 | T__157 | T__158 | T__159 | T__160 | T__161 | T__162 | T__163 | T__164 | T__165 | T__166 | T__167 | QUOTED_STRING | ID | SEMI | COLON | DOUBLEDOT | DOT | COMMA | EXPONENT | ASTERISK | AT_SIGN | RPAREN | LPAREN | RBRACK | LBRACK | PLUS | MINUS | DIVIDE | EQ | PERCENTAGE | LLABEL | RLABEL | ASSIGN | ARROW | VERTBAR | DOUBLEVERTBAR | NOT_EQ | LTH | LEQ | GTH | GEQ | NUMBER | QUOTE | WS | SL_COMMENT | ML_COMMENT | TYPE_ATTR | ROWTYPE_ATTR | NOTFOUND_ATTR | FOUND_ATTR | ISOPEN_ATTR | ROWCOUNT_ATTR | BULK_ROWCOUNT_ATTR | CHARSET_ATTR );";
        }
        public int specialStateTransition(int s, IntStream _input) throws NoViableAltException {
            IntStream input = _input;
        	int _s = s;
            switch ( s ) {
                    case 0 : 
                        int LA14_22 = input.LA(1);

                        s = -1;
                        if ( ((LA14_22>='\u0000' && LA14_22<='\uFFFF')) ) {s = 21;}

                        else s = 116;

                        if ( s>=0 ) return s;
                        break;
            }
            if (state.backtracking>0) {state.failed=true; return -1;}
            NoViableAltException nvae =
                new NoViableAltException(getDescription(), 14, _s, input);
            error(nvae);
            throw nvae;
        }
    }
 

}