/**
 * Set up the audio player with the provided player, icon, and image IDs.
 * @param {string} playerId - The ID of the audio element.
 * @param {string} iconId - The ID of the icon element.
 * @param {string} imageId - The ID of the image element.
 */
function setUpAudioPlayer(playerId, iconId, imageId) {
  // Get the DOM elements by their IDs
  const player = document.getElementById(playerId);
  const icon = document.getElementById(iconId);
  const speciesImage = document.getElementById(imageId);

  // Check if all elements are present
  if (!player || !icon || !speciesImage) {
    console.log('One of the elements is missing:', playerId, iconId, imageId);
    return;
  }

  // Add a click event listener to the species image to toggle play/pause
  speciesImage.addEventListener('click', function () {
    if (player.paused) {
      player.play();
      icon.innerHTML = '<i class="bi bi-pause-fill"></i>';
    } else {
      player.pause();
      icon.innerHTML = '<i class="bi bi-play-fill"></i>';
    }
  });

  // Add an event listener to reset the icon when the audio ends
  player.addEventListener('ended', function () {
    icon.innerHTML = '<i class="bi bi-play-fill"></i>';
  });

  console.log('Audio player set up for:', playerId, iconId, imageId);
}

/**
 * Initialize all audio players on the page.
 * This function finds all elements with the class 'detection_sound_image' and sets up the audio players for each.
 */
function initializePlayers() {
  // Select all elements with the class 'detection_sound_image'
  const players = document.querySelectorAll('.detection_sound_image');
  // Iterate through each player element and set up the audio player
  players.forEach(player => {
    const playerId = player.querySelector('audio').id;
    const iconId = player.querySelector('div.overlay_icon').id;
    const imageId = player.querySelector('img').id;
    setUpAudioPlayer(playerId, iconId, imageId);
  });
  console.log("Players initialized!");
}

/**
 * Check if the required number of audio player elements are present in the DOM.
 * If the required number of elements are present, initialize the players.
 * Otherwise, continue checking using requestAnimationFrame.
 * @param {number} targetLength - The expected number of elements with the class 'detection_sound_image'.
 */
function checkInitialization(targetLength) {
  const currentLength = document.querySelectorAll('.detection_sound_image').length;
  console.log(`Checking initialization: currentLength = ${currentLength}, targetLength = ${targetLength}`);

  // If the current number of elements matches or exceeds the target, initialize the players
  if (currentLength >= targetLength) {
    initializePlayers();
    console.log("All elements are present. Players initialized!");
  } else {
    // Continue checking until the required number of elements are present
    requestAnimationFrame(() => checkInitialization(targetLength));
    console.log("Players not found, rechecking...");
  }
}

// Document ready event to indicate that the document is fully loaded
$(document).ready(function() {
  console.log("Document ready, awaiting initialization signal...");
});

// Custom message handler to reinitialize players when new content is added by Shiny
$(document).ready(function() {
  Shiny.addCustomMessageHandler('newCardsAdded', function(message) {
    console.log("Custom message received:", message);
    const targetLength = message.targetLength;
    checkInitialization(targetLength); // Use the checkInitialization function
    console.log("New cards added, initialization checking started!");
  });
});
