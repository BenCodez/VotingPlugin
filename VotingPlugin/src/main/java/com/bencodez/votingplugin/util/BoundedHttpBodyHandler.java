package com.bencodez.votingplugin.util;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.http.HttpResponse;
import java.nio.ByteBuffer;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.Flow;

/** Completes only after the entire bounded HTTP body has been consumed. */
public final class BoundedHttpBodyHandler implements HttpResponse.BodyHandler<byte[]> {
	private final int maximumBytes;

	public BoundedHttpBodyHandler(int maximumBytes) {
		if (maximumBytes < 0) throw new IllegalArgumentException("maximumBytes must be non-negative");
		this.maximumBytes = maximumBytes;
	}

	@Override
	public HttpResponse.BodySubscriber<byte[]> apply(HttpResponse.ResponseInfo responseInfo) {
		return new Subscriber(maximumBytes);
	}

	private static final class Subscriber implements HttpResponse.BodySubscriber<byte[]> {
		private final int maximumBytes;
		private final ByteArrayOutputStream bytes;
		private final CompletableFuture<byte[]> result = new CompletableFuture<>();
		private Flow.Subscription subscription;

		private Subscriber(int maximumBytes) {
			this.maximumBytes = maximumBytes;
			this.bytes = new ByteArrayOutputStream(Math.min(maximumBytes, 8192));
		}

		@Override
		public CompletionStage<byte[]> getBody() {
			return result;
		}

		@Override
		public void onSubscribe(Flow.Subscription selected) {
			if (subscription != null) {
				selected.cancel();
				return;
			}
			subscription = selected;
			selected.request(1);
		}

		@Override
		public void onNext(List<ByteBuffer> items) {
			if (result.isDone()) return;
			for (ByteBuffer item : items) {
				int count = item.remaining();
				if (count > maximumBytes - bytes.size()) {
					subscription.cancel();
					result.completeExceptionally(new IOException("HTTP response body exceeds the limit"));
					return;
				}
				byte[] chunk = new byte[count];
				item.get(chunk);
				bytes.write(chunk, 0, chunk.length);
			}
			subscription.request(1);
		}

		@Override
		public void onError(Throwable failure) {
			result.completeExceptionally(failure);
		}

		@Override
		public void onComplete() {
			result.complete(bytes.toByteArray());
		}
	}
}
