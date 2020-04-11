/*
 * Copyright 2016-present Open Networking Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.onlab.util;

import com.google.common.testing.EqualsTester;
import org.apache.commons.lang3.RandomUtils;
import org.junit.Assert;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Random;

import static java.lang.Integer.max;
import static java.lang.String.format;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;

public class ImmutableByteSequenceTest {
    public static final int MIN_RAND_FIT_VALUE = 0xf;
    public static final int MAX_RAND_FIT_VALUE = 0x7fffffff;
    @Rule
    public ExpectedException thrown = ExpectedException.none();

    @Test
    public void testCopy() throws Exception {

        byte byteValue = (byte) 1;
        short shortValue = byteValue;
        int intValue = byteValue;
        long longValue = byteValue;
        byte[] arrayValue = new byte[64];
        arrayValue[63] = byteValue;
        ByteBuffer bufferValue = ByteBuffer.allocate(64).put(arrayValue);

        ImmutableByteSequence bsByte = ImmutableByteSequence.copyFrom(byteValue);
        ImmutableByteSequence bsShort = ImmutableByteSequence.copyFrom(shortValue);
        ImmutableByteSequence bsInt = ImmutableByteSequence.copyFrom(intValue);
        ImmutableByteSequence bsLong = ImmutableByteSequence.copyFrom(longValue);
        ImmutableByteSequence bsArray = ImmutableByteSequence.copyFrom(arrayValue);
        ImmutableByteSequence bsBuffer = ImmutableByteSequence.copyFrom(bufferValue);

        assertThat("byte sequence of a byte value must have size 1",
                   bsByte.size(), is(equalTo(1)));
        assertThat("byte sequence of a short value must have size 2",
                   bsShort.size(), is(equalTo(2)));
        assertThat("byte sequence of an int value must have size 4",
                   bsInt.size(), is(equalTo(4)));
        assertThat("byte sequence of a long value must have size 8",
                   bsLong.size(), is(equalTo(8)));
        assertThat("byte sequence of a byte array value must have same size of the array",
                   bsArray.size(), is(equalTo(arrayValue.length)));
        assertThat("byte sequence of a byte buffer value must have same size of the buffer",
                   bsBuffer.size(), is(equalTo(bufferValue.capacity())));

        String errStr = "incorrect byte sequence value";

        assertThat(errStr, bsByte.asArray()[0], is(equalTo(byteValue)));
        assertThat(errStr, bsShort.asArray()[1], is(equalTo(byteValue)));
        assertThat(errStr, bsInt.asArray()[3], is(equalTo(byteValue)));
        assertThat(errStr, bsLong.asArray()[7], is(equalTo(byteValue)));
        assertThat(errStr, bsArray.asArray()[63], is(equalTo(byteValue)));
        assertThat(errStr, bsBuffer.asArray()[63], is(equalTo(byteValue)));
    }

    @Test
    public void testEndianness() throws Exception {

        long longValue = RandomUtils.nextLong();

        // creates a new sequence from a big-endian buffer
        ByteBuffer bbBigEndian = ByteBuffer
                .allocate(8)
                .order(ByteOrder.BIG_ENDIAN)
                .putLong(longValue);
        ImmutableByteSequence bsBufferCopyBigEndian =
                ImmutableByteSequence.copyFrom(bbBigEndian);

        // creates a new sequence from a little-endian buffer
        ByteBuffer bbLittleEndian = ByteBuffer
                .allocate(8)
                .order(ByteOrder.LITTLE_ENDIAN)
                .putLong(longValue);
        ImmutableByteSequence bsBufferCopyLittleEndian =
                ImmutableByteSequence.copyFrom(bbLittleEndian);

        // creates a new sequence from primitive type
        ImmutableByteSequence bsLongCopy =
                ImmutableByteSequence.copyFrom(longValue);


        new EqualsTester()
                // big-endian byte array cannot be equal to little-endian array
                .addEqualityGroup(bbBigEndian.array())
                .addEqualityGroup(bbLittleEndian.array())
                // all byte sequences must be equal
                .addEqualityGroup(bsBufferCopyBigEndian,
                                  bsBufferCopyLittleEndian,
                                  bsLongCopy)
                // byte buffer views of all sequences must be equal
                .addEqualityGroup(bsBufferCopyBigEndian.asReadOnlyBuffer(),
                                  bsBufferCopyLittleEndian.asReadOnlyBuffer(),
                                  bsLongCopy.asReadOnlyBuffer())
                // byte buffer orders of all sequences must be ByteOrder.BIG_ENDIAN
                .addEqualityGroup(bsBufferCopyBigEndian.asReadOnlyBuffer().order(),
                                  bsBufferCopyLittleEndian.asReadOnlyBuffer().order(),
                                  bsLongCopy.asReadOnlyBuffer().order(),
                                  ByteOrder.BIG_ENDIAN)
                .testEquals();
    }

    @Test
    public void testBitSetMethods() throws Exception {
        // All zeros tests
        assertThat("3 bytes, all 0's",
                   ImmutableByteSequence.ofZeros(3),
                   is(equalTo(ImmutableByteSequence.copyFrom(
                           new byte[]{0, 0, 0}))));
        assertThat("3 bytes, all 0's via prefix",
                   ImmutableByteSequence.prefixZeros(3, 3 * Byte.SIZE),
                   is(equalTo(ImmutableByteSequence.copyFrom(
                           new byte[]{0, 0, 0}))));

        // All ones tests
        assertThat("3 bytes, all 1's",
                   ImmutableByteSequence.ofZeros(3),
                   is(equalTo(ImmutableByteSequence.copyFrom(
                           new byte[]{0, 0, 0}))));
        assertThat("3 bytes, all 1's via prefix",
                   ImmutableByteSequence.prefixOnes(3, 3 * Byte.SIZE),
                   is(equalTo(ImmutableByteSequence.copyFrom(
                           new byte[]{(byte) 0xff, (byte) 0xff, (byte) 0xff}))));

        // Zero prefix tests
        assertThat("2 bytes, prefixed with 5 0's",
                   ImmutableByteSequence.prefix(2, 5, (byte) 0),
                   is(equalTo(ImmutableByteSequence.copyFrom(
                           new byte[]{(byte) 0x7, (byte) 0xff}))));
        assertThat("4 bytes, prefixed with 16 0's",
                   ImmutableByteSequence.prefix(4, 16, (byte) 0),
                   is(equalTo(ImmutableByteSequence.copyFrom(
                           new byte[]{0, 0, (byte) 0xff, (byte) 0xff}))));
        assertThat("4 bytes, prefixed with 20 0's",
                   ImmutableByteSequence.prefix(4, 20, (byte) 0),
                   is(equalTo(ImmutableByteSequence.copyFrom(
                           new byte[]{0, 0, (byte) 0x0f, (byte) 0xff}))));
        assertThat("8 bytes, prefixed with 36 0's",
                   ImmutableByteSequence.prefixZeros(8, 38),
                   is(equalTo(ImmutableByteSequence.copyFrom(
                           new byte[]{0, 0, 0, 0, (byte) 0x03, (byte) 0xff, (byte) 0xff, (byte) 0xff}))));

        // Ones prefix tests
        assertThat("2 bytes, prefixed with 5 1's",
                   ImmutableByteSequence.prefix(2, 5, (byte) 0xff),
                   is(equalTo(ImmutableByteSequence.copyFrom(
                           new byte[]{(byte) 0xf8, 0}))));
        assertThat("4 bytes, prefixed with 16 1's",
                   ImmutableByteSequence.prefix(4, 16, (byte) 0xff),
                   is(equalTo(ImmutableByteSequence.copyFrom(
                           new byte[]{(byte) 0xff, (byte) 0xff, 0, 0}))));
        assertThat("4 bytes, prefixed with 20 1's",
                   ImmutableByteSequence.prefix(4, 20, (byte) 0xff),
                   is(equalTo(ImmutableByteSequence.copyFrom(
                           new byte[]{(byte) 0xff, (byte) 0xff, (byte) 0xf0, 0}))));
        assertThat("8 bytes, prefixed with 10 1's",
                   ImmutableByteSequence.prefixOnes(8, 10),
                   is(equalTo(ImmutableByteSequence.copyFrom(
                           new byte[]{(byte) 0xff, (byte) 0xc0, 0, 0, 0, 0, 0, 0}))));
    }

    @Test
    public void testBadPrefixVal() {
        thrown.expect(IllegalArgumentException.class);
        thrown.reportMissingExceptionWithMessage(
                "Expect IllegalArgumentException due to val = 0x7");
        ImmutableByteSequence.prefix(5, 10, (byte) 0x7);
    }

    @Test
    public void testMsbIndex() {
        assertThat("Value 0 should have MSB index -1",
                   ImmutableByteSequence.copyFrom(0).msbIndex(), is(-1));
        for (int i = 0; i < 63; i++) {
            long value = (long) Math.pow(2, i);
            assertThat(format("Value %d should have MSB index %d", value, i),
                       ImmutableByteSequence.copyFrom(value).msbIndex(), is(i));
        }
    }

    private void checkIllegalFit(ImmutableByteSequence bytes, int bitWidth) {
        try {
            bytes.fit(bitWidth);
            Assert.fail(format("Except ByteSequenceTrimException due to value = %s and bitWidth %d",
                               bytes.toString(), bitWidth));
        } catch (ImmutableByteSequence.ByteSequenceTrimException e) {
            // We expect this.
        }
    }

    private void checkLegalFit(ImmutableByteSequence bytes, int bitWidth)
            throws ImmutableByteSequence.ByteSequenceTrimException {
        ImmutableByteSequence fitBytes = bytes.fit(bitWidth);
        ImmutableByteSequence sameBytes = fitBytes.fit(bytes.size() * 8);
        assertThat(format("Fitted value %s (re-extended to %s) not equal to original value %s",
                          fitBytes, sameBytes, bytes),
                   sameBytes,
                   is(equalTo(bytes)));
    }

    @Test
    public void testFit() throws ImmutableByteSequence.ByteSequenceTrimException {
        // Test fit by forcing a given MSB index.
        for (int msbIndex = 0; msbIndex < 32; msbIndex++) {
            long value = (long) Math.pow(2, msbIndex);
            ImmutableByteSequence bytes = ImmutableByteSequence.copyFrom(value);
            checkLegalFit(bytes, msbIndex + 1);
            if (msbIndex != 0) {
                checkIllegalFit(bytes, msbIndex);
            }
        }
    }

    @Test
    public void testRandomFit() throws ImmutableByteSequence.ByteSequenceTrimException {
        // Test fit against the computed MSB index.
        Random random = new Random();
        for (int i = 0; i < 1000; i++) {
            int randValue = random.nextInt((MAX_RAND_FIT_VALUE - MIN_RAND_FIT_VALUE) + 1) + MIN_RAND_FIT_VALUE;
            ImmutableByteSequence bytes = ImmutableByteSequence.copyFrom((long) randValue);
            int msbIndex = bytes.msbIndex();
            // Truncate.
            checkIllegalFit(bytes, max(msbIndex - random.nextInt(16), 1));
            // Expand.
            checkLegalFit(bytes, msbIndex + 2 + random.nextInt(128));
            // Fit to same bit-width of original value.
            checkLegalFit(bytes, msbIndex + 1);
        }
    }

    @Test
    public void testBitwiseOperations() {
        Random random = new Random();
        long long1 = random.nextLong();
        long long2 = random.nextLong();

        ImmutableByteSequence bs1 = ImmutableByteSequence.copyFrom(long1);
        ImmutableByteSequence bs2 = ImmutableByteSequence.copyFrom(long2);

        ImmutableByteSequence andBs = bs1.bitwiseAnd(bs2);
        ImmutableByteSequence orBs = bs1.bitwiseOr(bs2);
        ImmutableByteSequence xorBs = bs1.bitwiseXor(bs2);

        assertThat("Invalid bitwise AND result",
                   andBs.asReadOnlyBuffer().getLong(), is(long1 & long2));
        assertThat("Invalid bitwise OR result",
                   orBs.asReadOnlyBuffer().getLong(), is(long1 | long2));
        assertThat("Invalid bitwise XOR result",
                   xorBs.asReadOnlyBuffer().getLong(), is(long1 ^ long2));
    }
}
